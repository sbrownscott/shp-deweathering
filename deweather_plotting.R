# Deweathering Analysis: Plotting and Interpretation
# Inputs:
#   - cambridge_no2_deweathered_covid_2020.csv 
#       (output from deweather_no2_covid_only.R)
#   - cambridge_combined_met_no2_traffic.csv (for baseline diurnal patterns)
# Outputs:
#   - observed_vs_deweathered_no2_covid_2020_smoothed.png
#   - no2_observed_minus_deweathered_covid_2020.png
#   - no2_vs_traffic_covid_2020.png
#   - daily_no2_vs_daily_traffic_covid_2020_linear.png
#   - weekday_diurnal_no2_faceted.png

# Load libraries
library(tidyverse)
library(lubridate)
library(mgcv)

# Read the deweathered COVID NO2 CSV
dw_covid_data <- read_csv("combined_data/cambridge_no2_deweathered_covid_2020.csv",
                          col_types = cols(
                            datetime = col_datetime(format = ""),
                            no2_obs = col_double(),
                            no2_dw = col_double(),
                            ws = col_double(),
                            wd = col_double(),
                            air_temp = col_double()
                          ))

# Smoothed time series plot of observed vs deweathered NO2
ggplot(dw_covid_data, aes(x = datetime)) +
  geom_smooth(aes(y = no2_obs, color = "Observed NO2"),
              method = "loess", se = FALSE, span = 0.2) +
  geom_smooth(aes(y = no2_dw, color = "Deweathered NO2"),
              method = "loess", se = FALSE, span = 0.2) +
  geom_vline(xintercept = as.POSIXct("2020-03-16"),
             linetype = "dashed", color = "darkgreen") +
  geom_vline(xintercept = as.POSIXct("2020-03-23"),
             linetype = "dashed", color = "black") +
  annotate("text", x = as.POSIXct("2020-03-16"),
           y = max(dw_covid_data$no2_obs, na.rm = TRUE),
           label = "Non-essential travel advised",
           angle = 0, vjust = -0.5, hjust = 1, color = "darkgreen") +
  annotate("text", x = as.POSIXct("2020-03-23"),
           y = max(dw_covid_data$no2_obs, na.rm = TRUE),
           label = "Lockdown start", angle = 0, vjust = -0.5, hjust = 0) +
  scale_color_manual(values = c("Observed NO2" = "blue",
                                "Deweathered NO2" = "red")) +
  labs(
    #title = "Observed vs Deweathered NO₂ – Smoothed Trends (Cambridge Jan–Apr 2020)",
    x = "Date",
    y = "NO₂ (µg/m³)",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  "graphs/observed_vs_deweathered_no2_covid_2020_smoothed.png",
  width = 10,
  height = 5,
  dpi = 300
)

# Plot of observed minus deweathered NO2 over time: not in report
dw_covid_data %>%
  mutate(diff = no2_obs - no2_dw) %>%
  ggplot(aes(x = datetime, y = diff)) +
  geom_smooth(aes(y = diff), method = "loess", se = TRUE, 
              span = 0.2, color = "purple") +
  geom_line(aes(y = diff), color = "purple", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed")

ggsave(
  "graphs/no2_observed_minus_deweathered_covid_2020.png",
  width = 10,
  height = 5,
  dpi = 300
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


# Merge NO2 and traffic data
combined_data <- dw_covid_data %>%
  left_join(traffic_data, by = "datetime") %>%
  drop_na(no2_dw, traffic_count)

daily_data <- combined_data %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(
    hours_available = n(),
    no2_obs_daily = mean(no2_obs, na.rm = TRUE),
    no2_dw_daily = mean(no2_dw, na.rm = TRUE),
    traffic_daily = mean(traffic_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(hours_available >= 18)

# Plot NO2 vs Traffic
ggplot(combined_data, aes(x = traffic_count)) +
  geom_smooth(aes(y = no2_obs, color = "Observed NO2"),
              method = "loess", se = FALSE, span = 0.3) +
  geom_smooth(aes(y = no2_dw, color = "Deweathered NO2"),
              method = "loess", se = FALSE, span = 0.3) +
  scale_color_manual(values = c("Observed NO2" = "blue",
                                "Deweathered NO2" = "red")) +
  labs(
    #title = "NO₂ vs Traffic Counts – Cambridge Jan–Apr 2020",
    x = "Traffic Count",
    y = "NO₂ (µg/m³)",
    color = "Legend"
  ) +
  theme_minimal()

ggsave(
  "graphs/no2_vs_traffic_covid_2020.png",
  width = 10,
  height = 5,
  dpi = 300
)

daily_data <- daily_data %>%
  mutate(period = case_when(
    date < as.Date("2020-03-23") ~ "Pre-COVID",
    TRUE ~ "Lockdown"
  ))

eq_daily_obs <- sprintf("Observed: y = %.3f + %.5fx",
                        coef(lm_daily_obs)[1], coef(lm_daily_obs)[2])
eq_daily_dw <- sprintf("Deweathered: y = %.3f + %.5fx",
                       coef(lm_daily_dw)[1], coef(lm_daily_dw)[2])

annot_x_daily <- min(daily_data$traffic_daily, na.rm = TRUE)
y_max_daily <- max(c(daily_data$no2_obs_daily, daily_data$no2_dw_daily), na.rm = TRUE)
y_span_daily <- diff(range(c(daily_data$no2_obs_daily, daily_data$no2_dw_daily),
                           na.rm = TRUE))

# Plot of daily mean NO2 vs daily mean traffic, with linear regression lines and equations
ggplot(daily_data, aes(x = traffic_daily)) +
  geom_point(aes(y = no2_obs_daily, color = "Observed NO₂", shape = period),
             alpha = 0.6) +
  geom_point(aes(y = no2_dw_daily, color = "Deweathered NO₂", shape = period),
             alpha = 0.6) +
  geom_smooth(aes(y = no2_obs_daily, color = "Observed NO₂"),
              method = "lm", se = TRUE) +
  geom_smooth(aes(y = no2_dw_daily, color = "Deweathered NO₂"),
              method = "lm", se = TRUE) +
  annotate("text", x = annot_x_daily, y = y_max_daily,
           label = eq_daily_obs, hjust = 0, color = "blue", size = 3.8) +
  annotate("text", x = annot_x_daily, y = y_max_daily - 0.08 * y_span_daily,
           label = eq_daily_dw, hjust = 0, color = "red", size = 3.8) +
  scale_color_manual(values = c("Observed NO₂" = "blue",
                                "Deweathered NO₂" = "red")) +
  scale_shape_manual(values = c("Pre-COVID" = 16,
                                "Lockdown" = 17)) +
  labs(
    x = "Daily Mean Traffic Count",
    y = "Daily Mean NO₂ (µg m⁻³)",
    color = "NO₂ Type",
    shape = "Period"
  ) +
  theme_minimal()

ggsave(
  "graphs/daily_no2_vs_daily_traffic_covid_2020_linear.png",
  width = 10,
  height = 5,
  dpi = 300
)

# Daily means by vehicle type
# Re-read full traffic data to get vehicle split
traffic_full <- read_csv("data/east_road_traffic.csv") %>%
  mutate(
    datetime = dmy_hms(paste(Date, Time))
  ) %>%
  group_by(datetime) %>%   # sum in + out
  summarise(
    Car        = sum(Car, na.rm = TRUE),
    Motorbike = sum(Motorbike, na.rm = TRUE),
    Bus        = sum(Bus, na.rm = TRUE),
    OGV1       = sum(OGV1, na.rm = TRUE),
    OGV2       = sum(OGV2, na.rm = TRUE),
    LGV        = sum(LGV, na.rm = TRUE),
    .groups = "drop"
  )

# Merge vehicle split with NO2
combined_veh <- dw_covid_data %>%
  left_join(traffic_full, by = "datetime") %>%
  drop_na(no2_dw, Car, LGV)

# Daily aggregation
daily_veh <- combined_veh %>%
  mutate(
    date = as.Date(datetime),
    weekday = wday(date, label = TRUE),
    day_type = if_else(weekday %in% c("Sat", "Sun"),
                       "Weekend", "Weekday")
  ) %>%
  group_by(date, day_type) %>%
  summarise(
    no2_obs = mean(no2_obs, na.rm = TRUE),
    no2_dw  = mean(no2_dw,  na.rm = TRUE),
    Car     = mean(Car, na.rm = TRUE),
    LGV     = mean(LGV, na.rm = TRUE),
    .groups = "drop"
  )

pre_covid_end <- as.Date("2020-03-15")
lockdown_start <- as.Date("2020-03-23")
post_covid_end <- as.Date("2020-05-31")

daily_data <- combined_data %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(
    traffic = mean(traffic_count, na.rm = TRUE),
    no2_obs = mean(no2_obs, na.rm = TRUE),
    no2_dw  = mean(no2_dw, na.rm = TRUE),
    .groups = "drop"
  ) 
  
daily_data <- daily_data %>%
  mutate(
    period = case_when(
      date <= pre_covid_end ~ "Pre-COVID",
      date >= lockdown_start & date <= post_covid_end ~ "Lockdown",
      TRUE ~ NA_character_
    )
  ) %>%
  drop_na(period)

period_means <- daily_data %>%
  group_by(period) %>%
  summarise(
    traffic_mean = mean(traffic),
    no2_obs_mean = mean(no2_obs),
    no2_dw_mean  = mean(no2_dw),
    .groups = "drop"
  )

print(period_means)

change <- period_means %>%
  summarise(
    traffic_drop = traffic_mean[period == "Lockdown"] -
                   traffic_mean[period == "Pre-COVID"],

    traffic_drop_pct = 100 * traffic_drop /
                       traffic_mean[period == "Pre-COVID"],

    no2_obs_drop = no2_obs_mean[period == "Lockdown"] -
                   no2_obs_mean[period == "Pre-COVID"],

    no2_dw_drop = no2_dw_mean[period == "Lockdown"] -
                  no2_dw_mean[period == "Pre-COVID"]
  )

print(change)

slope_dw <- coef(lm_dw)["traffic_count"]
slope_obs <- coef(lm_obs)["traffic_count"]


# Weekday Diurnal NO₂ Patterns: Raw vs Deweathered, Pre-COVID vs COVID
# Prepare raw NO2 from combined_data (repeated from previous section for clarity)
df_raw <- combined_data %>%
  mutate(
    date = as.Date(datetime),
    hour = hour(datetime),
    weekday = wday(date, label = TRUE)
  ) %>%
  filter(!weekday %in% c("Sat", "Sun"))  # weekdays only

# Pre-COVID: 1 Jan – 15 Mar 2020
pre_covid_raw <- df_raw %>% filter(date >= as.Date("2020-01-01"), date <= pre_covid_end)

# COVID period: 23 Mar – 31 May 2020
covid_raw <- df_raw %>% filter(date >= lockdown_start, date <= post_covid_end)

# Deweathered NO2: already in combined_data (no2_dw)
pre_covid_dw <- pre_covid_raw
covid_dw <- covid_raw

# Function to compute hourly means and SD
compute_diurnal <- function(data, no2_col, period_label, type_label) {
  data %>%
    group_by(hour) %>%
    summarise(
      mean_no2 = mean(.data[[no2_col]], na.rm = TRUE),
      sd_no2 = sd(.data[[no2_col]], na.rm = TRUE)
    ) %>%
    mutate(period = period_label, type = type_label)
}

# Compute diurnal averages
diurnal_pre_raw <- compute_diurnal(pre_covid_raw, "no2_obs", "Pre-COVID", "Raw")
diurnal_covid_raw <- compute_diurnal(covid_raw, "no2_obs", "COVID", "Raw")

diurnal_pre_dw <- compute_diurnal(pre_covid_dw, "no2_dw", "Pre-COVID", "Deweathered")
diurnal_covid_dw <- compute_diurnal(covid_dw, "no2_dw", "COVID", "Deweathered")

# Combine raw and deweathered diurnal data for faceting
diurnal_all <- bind_rows(diurnal_pre_raw, diurnal_covid_raw,
                         diurnal_pre_dw, diurnal_covid_dw) %>%
  mutate(type = factor(type, levels = c("Raw", "Deweathered")))

# 2014–2019 Baseline Diurnal (Weekdays)

# reintroducted to avoid overwriting
baseline_data <- read_csv("combined_data/cambridge_combined_met_no2_traffic.csv") %>%
  mutate(
    datetime = dmy(date) + hms(time),
    date = as.Date(datetime),
    hour = hour(datetime),
    weekday = wday(date, label = TRUE)
  ) %>%
  filter(
    year(datetime) %in% 2014:2019,
    !weekday %in% c("Sat", "Sun")
  )

# Define equivalent periods
baseline_pre <- baseline_data %>%
  filter(format(date, "%m-%d") >= "01-01",
         format(date, "%m-%d") <= "03-22")

baseline_post <- baseline_data %>%
  filter(format(date, "%m-%d") >= "03-23",
         format(date, "%m-%d") <= "05-31")

# Compute hourly means
baseline_pre_diurnal <- baseline_pre %>%
  group_by(hour) %>%
  summarise(mean_no2 = mean(no2, na.rm = TRUE)) %>%
  mutate(period = "01/01 - 22/03 (2014–2019)",
         type = "Raw")

baseline_post_diurnal <- baseline_post %>%
  group_by(hour) %>%
  summarise(mean_no2 = mean(no2, na.rm = TRUE)) %>%
  mutate(period = "23/03 - 31/05 (2014–2019)",
         type = "Raw")

diurnal_all_extended <- bind_rows(
  diurnal_all,
  baseline_pre_diurnal,
  baseline_post_diurnal
)

diurnal_all_extended <- diurnal_all_extended %>%
  mutate(type = factor(type, levels = c("Raw", "Deweathered")))

ggplot(diurnal_all_extended,
       aes(x = hour, y = mean_no2,
           color = period, linetype = period)) +

  geom_line(size = 1) +

  facet_wrap(~type, ncol = 2, scales = "free_y") +

  scale_color_manual(values = c(
    "Pre-COVID" = "blue",
    "COVID" = "red",
    "01/01 - 22/03 (2014–2019)" = "blue",
    "23/03 - 31/05 (2014–2019)" = "red"
  )) +

  scale_linetype_manual(values = c(
    "Pre-COVID" = "solid",
    "COVID" = "solid",
    "01/01 - 22/03 (2014–2019)" = "dashed",
    "23/03 - 31/05 (2014–2019)" = "dashed"
  )) +

  labs(
    title = "Weekday Diurnal NO₂: 2020 vs 2014–2019 Baseline",
    x = "Hour of Day",
    y = "NO₂ (µg/m³)",
    color = "Period",
    linetype = "Period"
  ) +

  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold")
  )

# Save the faceted plot
ggsave(
  "graphs/weekday_diurnal_no2_faceted.png",
  width = 10,
  height = 5,
  dpi = 300
)

print("end")