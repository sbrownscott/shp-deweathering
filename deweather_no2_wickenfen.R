# Deweathering NO₂ at Wicken Fen (2014–2020)
# Inputs:
#   - combined_data/wickenfen_combined_met_no2.csv
# Outputs:
#   - combined_data/wickenfen_no2_deweathered_covid_2020.csv
#   - graphs/wickenfen_observed_vs_deweathered_2020.png

# Load libraries
library(tidyverse)
library(lubridate)
library(deweather)

# Read & clean data
wickenfen <- read_csv(
  "combined_data/wickenfen_combined_met_no2.csv",
  col_types = cols(
    date = col_character(),
    time = col_character(),
    no2  = col_double(),
    ws   = col_character(),
    wd   = col_character(),
    temp = col_character()
  )
) %>%
  # convert "No data" → NA and numeric
  mutate(
    ws = as.numeric(if_else(ws == "No data", NA_character_, ws)),
    wd = as.numeric(if_else(wd == "No data", NA_character_, wd)),
    temp = as.numeric(if_else(temp == "No data", NA_character_, temp)),
    # fix 24:00:00
    time_orig = time,
    time = if_else(time == "24:00:00", "00:00:00", time),
    datetime = dmy(date) + hms(time),
    # add a day if original was 24:00:00
    datetime = if_else(time_orig == "24:00:00", datetime + days(1), datetime),
    air_temp = temp,
    hour = factor(hour(datetime), levels = 0:23),
    weekday = factor(wday(datetime, label = TRUE)),
    trend = as.numeric(difftime(datetime, min(datetime, na.rm = TRUE), units = "days"))
  ) %>%
  mutate(
    wd_sin = sin(wd * pi / 180),
    wd_cos = cos(wd * pi / 180)
  ) %>%
  drop_na(datetime, no2, ws, wd, air_temp)

# Training data (2014–2019)
training_data <- wickenfen %>%
  filter(year(datetime) %in% 2014:2019) %>%
  mutate(
    hour = factor(hour(datetime)),
    weekday = factor(wday(datetime, label = TRUE))
  ) %>%
  droplevels()

# Build deweathering model
dw_model <- build_dw_model(
  data = training_data,
  pollutant = "no2",
  vars = c("trend", "ws", "wd_sin", "wd_cos", "hour", "weekday", "air_temp"),
  engine = "xgboost",
  trees = 300,
  tree_depth = 6,
  learn_rate = 0.05,
  .date = "datetime"
)

# Apply model to COVID period (Jan–Apr 2020)
covid_data <- wickenfen %>%
  filter(datetime >= as.POSIXct("2020-01-01", tz = "UTC"),
         datetime <= as.POSIXct("2020-04-30", tz = "UTC"))

dw_covid <- simulate_dw_met(
  dw_model,
  newdata = covid_data,
  n = 100
) %>%
  rename(no2_dw = no2) %>%
  mutate(datetime = covid_data$datetime)  # add datetime back

# Export to CSV
# Add observed NO2 from the original COVID data
dw_covid_out <- dw_covid %>%
  # add observed NO2 and meteorology from original covid_data
  mutate(
    no2_obs = covid_data$no2,
    ws      = covid_data$ws,
    wd      = covid_data$wd,
    air_temp = covid_data$air_temp
  ) %>%
  select(datetime, no2_obs, no2_dw, ws, wd, air_temp)

# Write to CSV
write_csv(
  dw_covid_out,
  "combined_data/wickenfen_no2_deweathered_covid_2020.csv"
)

message("✓ Deweathered NO₂ for Jan–Apr 2020 exported successfully")

wickenfen_summary <- dw_covid_out %>%
  summarise(
    mean_dw = mean(no2_dw, na.rm = TRUE),
    median_dw = median(no2_dw, na.rm = TRUE),
    sd_dw = sd(no2_dw, na.rm = TRUE)
  )
print(wickenfen_summary)

# Plot observed vs deweathered NO₂
ggplot(dw_covid_out, aes(x = datetime)) +
  geom_smooth(aes(y = no2_obs, colour = "Observed NO₂"),
              method = "loess", se = FALSE, span = 0.2) +
  geom_smooth(aes(y = no2_dw, colour = "Deweathered NO₂"),
              method = "loess", se = FALSE, span = 0.2) +
  geom_vline(xintercept = as.POSIXct("2020-03-23"),
             linetype = "dashed") +
  scale_colour_manual(values = c("Observed NO₂" = "blue",
                                 "Deweathered NO₂" = "red")) +
  labs(
    #title = "Observed vs Deweathered NO₂ – Wicken Fen (Jan–Apr 2020)",
    x = "Date",
    y = "NO₂ (µg m⁻³)",
    colour = ""
  ) +
  theme_minimal()

ggsave("graphs/wickenfen_observed_vs_deweathered_2020.png",
       width = 10, height = 5, dpi = 300)


pre_end <- as.Date("2020-03-15")
lock_start <- as.Date("2020-03-23")
lock_end <- as.Date("2020-04-30")

# Apply offset using pre-COVID mean

pre_period <- dw_covid_out %>%
  filter(as.Date(datetime) <= as.Date("2020-03-15"))

offset <- mean(pre_period$no2_obs, na.rm = TRUE) -
          mean(pre_period$no2_dw, na.rm = TRUE)

# Keep both versions
dw_covid_out <- dw_covid_out %>%
  mutate(
    no2_dw_raw = no2_dw,
    no2_dw_adj = no2_dw + offset
  )

cat("Offset applied:", offset, "\n")

# Plot observed vs deweathered (raw and adjusted) NO₂
ggplot(dw_covid_out, aes(x = datetime)) +
  geom_smooth(aes(y = no2_obs, colour = "Observed NO₂"),
              method = "loess", se = FALSE, span = 0.2) +
  geom_smooth(aes(y = no2_dw_raw, colour = "Deweathered (Raw)"),
              method = "loess", se = FALSE, span = 0.2) +
  geom_smooth(aes(y = no2_dw_adj, colour = "Deweathered (Adjusted)"),
              method = "loess", se = FALSE, span = 0.2) +
  geom_vline(xintercept = as.POSIXct("2020-03-23"),
             linetype = "dashed") +
  scale_colour_manual(values = c(
    "Observed NO₂" = "blue",
    "Deweathered (Raw)" = "red",
    "Deweathered (Adjusted)" = "#f67ea2"
  )) +
  labs(
    #title = "Observed vs Deweathered NO₂ (Raw and Adjusted) – Wicken Fen (Jan–Apr 2020)",
    x = "Date",
    y = "NO₂ (µg m⁻³)",
    colour = ""
  ) +
  theme_minimal()

ggsave("graphs/wickenfen_observed_vs_deweathered_adjusted_2020.png",
       width = 10, height = 5, dpi = 300)

cat("Mean no2_dw_adj:", mean(dw_covid_out$no2_dw_adj, na.rm = TRUE), "\n")

# data for report
mean_obs_pre <- mean(pre_period$no2_obs, na.rm = TRUE)
mean_dw_pre <- mean(pre_period$no2_dw, na.rm = TRUE)

round(mean_obs_pre, 1)
round(mean_dw_pre, 1)
round(offset, 1)

if(mean_dw_pre < mean_obs_pre) {
  cat("underestimation of pre-COVID NO₂ by", round(offset, 1), "µg m⁻³\n")  
} else {
  cat("overestimation of pre-COVID NO₂ by", round(-offset, 1), "µg m⁻³\n")  
}

pre_data <- dw_covid_out %>%
  filter(as.Date(datetime) <= pre_end)

post_data <- dw_covid_out %>%
  filter(as.Date(datetime) >= lock_start,
         as.Date(datetime) <= lock_end)

summary_table <- tibble(
  Type = c("Observed NO2", "Deweathered NO2", "Adjusted Deweathered NO2"),
  
  Pre = c(
    mean(pre_data$no2_obs, na.rm = TRUE),
    mean(pre_data$no2_dw_raw, na.rm = TRUE),
    mean(pre_data$no2_dw_adj, na.rm = TRUE)
  ),
  
  Post = c(
    mean(post_data$no2_obs, na.rm = TRUE),
    mean(post_data$no2_dw_raw, na.rm = TRUE),
    mean(post_data$no2_dw_adj, na.rm = TRUE)
  )
) %>%
  mutate(
    Reduction = (Pre - Post) / Pre * 100
  )

summary_table <- summary_table %>%
  mutate(
    Pre = round(Pre, 1),
    Post = round(Post, 1),
    Reduction = round(Reduction, 1)
  )

summary_table