# Deweathering NO₂ at Cambridge: Offsetting Method
# Inputs:
#   - combined_data/cambridge_no2_deweathered_covid_2020.csv
#   - combined_data/wickenfen_no2_deweathered_covid_2020.csv
#   - data/east_road_traffic.csv
# Outputs:
#   - combined_data/cambridge_no2_deweathered_adjusted_2020.csv
#   - graphs/observed_vs_deweathered_adjusted_no2_covid_2020_smoothed.png
#   - graphs/traffic_counts_vs_no2_covid_2020.png
#   - graphs/observed_and_traffic_no2_covid_2020_combined.png

library(tidyverse)
library(lubridate)
library(patchwork)

# Load urban data

cambridge <- read_csv(
  "combined_data/cambridge_no2_deweathered_covid_2020.csv",
  col_types = cols(
    datetime = col_datetime(),
    no2_obs = col_double(),
    no2_dw = col_double()
  )
)

# Load rural data

wickenfen <- read_csv(
  "combined_data/wickenfen_no2_deweathered_covid_2020.csv",
  col_types = cols(
    datetime = col_datetime(),
    no2_obs = col_double(),
    no2_dw = col_double()
  )
)

# Load traffic data

traffic_data <- read_csv("data/east_road_traffic.csv")
colnames(traffic_data)

traffic_data <- traffic_data %>%
  mutate(
    datetime = dmy_hms(paste(Date, Time)),   # combine Date and Time
    traffic_count =
      Car + Motorbike + Bus + OGV1 + OGV2 + LGV  # sum vehicle types
  ) %>%
  select(datetime, traffic_count)

cambridge <- cambridge %>%
  left_join(traffic_data, by = "datetime")

# Define pre-COVID baseline period

pre_covid_end <- as.POSIXct("2020-03-15", tz = "UTC")

# Calculate rural baseline
rural_baseline <- wickenfen %>%
  filter(datetime <= pre_covid_end) %>%
  summarise(mean_rural = mean(no2_obs, na.rm = TRUE)) %>%
  pull(mean_rural)

print(paste("Rural baseline:", round(rural_baseline, 2)))

# Calculate urban increments
urban_increment_raw <- cambridge %>%
  filter(datetime <= pre_covid_end) %>%
  summarise(mean_raw = mean(no2_obs, na.rm = TRUE)) %>%
  pull(mean_raw) - rural_baseline

urban_increment_dw <- cambridge %>%
  filter(datetime <= pre_covid_end) %>%
  summarise(mean_dw = mean(no2_dw, na.rm = TRUE)) %>%
  pull(mean_dw) - rural_baseline

print(paste("Raw urban increment:", round(urban_increment_raw, 2)))
print(paste("Deweathered urban increment:", round(urban_increment_dw, 2)))

# Calculate offset
offset <- urban_increment_raw - urban_increment_dw

print(paste("Offset applied:", round(offset, 2)))

# Apply offset to deweathered NO2
cambridge <- cambridge %>%
  mutate(no2_dw_adjusted = no2_dw + offset)

# Save adjusted dataset
write_csv(
  cambridge,
  "combined_data/cambridge_no2_deweathered_adjusted_2020.csv"
)

# Convert to daily means
cambridge_daily <- cambridge %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(
    no2_obs = mean(no2_obs, na.rm = TRUE),
    no2_dw = mean(no2_dw, na.rm = TRUE),
    no2_dw_adjusted = mean(no2_dw_adjusted, na.rm = TRUE),
    traffic_count = mean(traffic_count, na.rm = TRUE)
  )

print(paste("Rural baseline:", round(rural_baseline, 2)))
print(paste("Raw urban increment:", round(urban_increment_raw, 2)))
print(paste("Deweathered urban increment:", round(urban_increment_dw, 2)))
print(paste("Offset applied:", round(offset, 2)))

model_obs <- lm(no2_obs ~ traffic_count, data = cambridge_daily)
model_dw <- lm(no2_dw ~ traffic_count, data = cambridge_daily)
model_dw_adj <- lm(no2_dw_adjusted ~ traffic_count, data = cambridge_daily)

coef(model_obs)
coef(model_dw)
coef(model_dw_adj)

# Dataset summary
cat("\n--- Dataset Summary ---\n")

cambridge_daily %>%
  summarise(
    mean_traffic = mean(traffic_count, na.rm = TRUE),
    mean_no2_obs = mean(no2_obs, na.rm = TRUE),
    mean_no2_dw = mean(no2_dw, na.rm = TRUE),
    mean_no2_adj = mean(no2_dw_adjusted, na.rm = TRUE),
    sd_no2_obs = sd(no2_obs, na.rm = TRUE)
  ) %>%
  print()

# Pre vs lockdown comparison
lockdown_start <- as.Date("2020-03-23")

cambridge_daily %>%
  mutate(period = if_else(date < lockdown_start,
                          "Pre-lockdown", "Lockdown")) %>%
  group_by(period) %>%
  summarise(
    mean_traffic = mean(traffic_count, na.rm = TRUE),
    mean_no2_obs = mean(no2_obs, na.rm = TRUE),
    mean_no2_dw = mean(no2_dw, na.rm = TRUE),
    mean_no2_adj = mean(no2_dw_adjusted, na.rm = TRUE)
  ) %>%
  print()

# Plot 1: Observed vs Deweathered vs Adjusted
p1 <- ggplot(cambridge, aes(x = datetime)) +

  geom_smooth(aes(y = no2_obs, color = "Observed NO2"),
              method = "loess", se = FALSE, span = 0.2) +

  geom_smooth(aes(y = no2_dw, color = "Deweathered NO2"),
              method = "loess", se = FALSE, span = 0.2) +

  geom_smooth(aes(y = no2_dw_adjusted, color = "Adjusted Deweathered NO2"),
              method = "loess", se = FALSE, span = 0.2) +

  geom_vline(xintercept = as.POSIXct("2020-03-16"),
             linetype = "dashed", color = "darkgreen") +

  geom_vline(xintercept = as.POSIXct("2020-03-23"),
             linetype = "dashed", color = "black") +

  annotate("text",
           x = as.POSIXct("2020-03-16"),
           y = max(cambridge$no2_obs, na.rm = TRUE),
           label = "Non-essential travel advised",
           vjust = -0.5, hjust = 1, color = "darkgreen") +

  annotate("text",
           x = as.POSIXct("2020-03-23"),
           y = max(cambridge$no2_obs, na.rm = TRUE),
           label = "Lockdown start",
           vjust = -0.5, hjust = 0) +

  scale_color_manual(values = c(
    "Observed NO2" = "blue",
    "Deweathered NO2" = "red",
    "Adjusted Deweathered NO2" = "#f67ea2"
  )) +

  labs(
    #title = "Observed vs Deweathered NO₂ – Smoothed Trends (Cambridge Jan–Apr 2020)",
    x = "Date",
    y = "NO₂ (µg/m³)",
    color = "Legend"
  ) +

  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

ggsave(
  "graphs/observed_vs_deweathered_adjusted_no2_covid_2020_smoothed.png",
  p1,
  width = 10,
  height = 5,
  dpi = 300
)

# Calculate urban increments (daily)
cambridge_daily <- cambridge_daily %>%
  mutate(
    increment_raw = no2_obs - rural_baseline,
    increment_dw = no2_dw - rural_baseline,
    increment_dw_adj = no2_dw_adjusted - rural_baseline
  )

# Pre vs lockdown comparison (daily)
cambridge_daily <- cambridge_daily %>%
  mutate(
    period = if_else(date < as.Date("2020-03-23"),
                     "Pre-lockdown", "Lockdown")
  )

# Plot 2 (deleted): Daily increments vs traffic counts

# Plot 3: Traffic counts vs NO2
p3 <- ggplot(cambridge_daily, aes(x = traffic_count)) +

  geom_point(aes(y = no2_obs, color = "Observed NO2"), alpha = 0.5) +
  geom_point(aes(y = no2_dw, color = "Deweathered NO2"), alpha = 0.5) +
  geom_point(aes(y = no2_dw_adjusted, color = "Adjusted Deweathered NO2"), alpha = 0.5) +

  geom_smooth(aes(y = no2_obs, color = "Observed NO2"), method = "lm", se = FALSE) +
  geom_smooth(aes(y = no2_dw, color = "Deweathered NO2"), method = "lm", se = FALSE) +
  geom_smooth(aes(y = no2_dw_adjusted, color = "Adjusted Deweathered NO2"), method = "lm", se = FALSE) +

  scale_color_manual(values = c(
    "Observed NO2" = "blue",
    "Deweathered NO2" = "red",
    "Adjusted Deweathered NO2" = "#f67ea2"
  )) +

  labs(
    #title = "Traffic Counts vs NO₂ (Cambridge Jan–Apr 2020)",
    x = "Traffic Count (Daily Average)",
    y = "NO₂ (µg/m³)",
    color = "Legend"
  ) +
  theme_minimal()

print(p3)

ggsave(
  "graphs/traffic_counts_vs_no2_covid_2020.png",
  p3,
  width = 10,
  height = 5,
  dpi = 300
)

# Combined plot: side-by-side with shared legend
combined_plot <- (p1 | p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(
  "graphs/observed_and_traffic_no2_covid_2020_combined.png",
  combined_plot,
  width = 16,
  height = 6,
  dpi = 300
)

model_pre <- lm(no2_dw_adjusted ~ traffic_count,
                data = filter(cambridge_daily, period == "Pre-lockdown"))

model_post <- lm(no2_dw_adjusted ~ traffic_count,
                 data = filter(cambridge_daily, period == "Lockdown"))

coef(model_pre)
coef(model_post)