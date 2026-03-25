# Deweathering NO2 for Cambridge (COVID period only)
# Input:
#   - cambridge_combined_met_no2_traffic.csv (for 2014–2020 meteorology and NO2)
# Output:
#   - cambridge_no2_deweathered_covid_2020.csv
#       (deweathered NO2 for Jan–Apr 2020, trained on 2014–2019 data)
#   - deweather_importance.png (variable importance plot for the model)

# Load libraries
library(tidyverse)
library(lubridate)
library(deweather)

# Read & clean data
cambridge <- read_csv(
  "combined_data/cambridge_combined_met_no2_traffic.csv",
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
    trend = as.numeric(difftime(datetime, min(datetime, na.rm = TRUE),
                                units = "days"))
  ) %>%
  mutate(
    wd_sin = sin(wd * pi / 180),
    wd_cos = cos(wd * pi / 180)
  ) %>%
  drop_na(datetime, no2, ws, wd, air_temp)

# Remove days with <18 valid hours
cambridge <- cambridge %>%
  mutate(date_day = as.Date(datetime)) %>%
  group_by(date_day) %>%
  mutate(hours_available = n()) %>%
  ungroup() %>%
  filter(hours_available >= 18) %>%
  select(-date_day, -hours_available)

# Training data (2014–2019)
training_data <- cambridge %>%
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
covid_data <- cambridge %>%
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
  "combined_data/cambridge_no2_deweathered_covid_2020.csv"
)

message("✓ Deweathered NO₂ for Jan–Apr 2020 exported successfully")

# Summary statistics comparing observed vs deweathered NO2
cambridge_summary <- dw_covid_out %>%
  summarise(
    mean_dw = mean(no2_dw, na.rm = TRUE),
    median_dw = median(no2_dw, na.rm = TRUE),
    sd_dw = sd(no2_dw, na.rm = TRUE)
  )
print(cambridge_summary)

dw_covid_out <- dw_covid_out %>%
  mutate(diff = no2_dw - no2_obs)

dw_covid_out %>%
  summarise(
    mean_obs  = mean(no2_obs, na.rm = TRUE),
    mean_dw   = mean(no2_dw, na.rm = TRUE),
    mean_diff = mean(diff, na.rm = TRUE),
    percent_diff = 100 * mean_diff / mean_dw
  )

dw_covid_out %>%
  filter(datetime >= as.POSIXct("2020-03-23", tz = "UTC")) %>%
  summarise(
    mean_obs  = mean(no2_obs, na.rm = TRUE),
    mean_dw   = mean(no2_dw, na.rm = TRUE),
    mean_diff = mean(no2_dw - no2_obs, na.rm = TRUE),
    percent_diff = 100 * mean_diff / mean_dw
  )
  
dw_model


# Plot of importance of variables in the deweathering model
imp_plot <- plot_dw_importance(
  dw_model,
  aggregate_factors = TRUE,
  sort = TRUE
) +
  labs(
    #title = "Variable Importance in Deweathering Model",
    y = "Relative Importance (%)",
    x = NULL
  ) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme(panel.grid = element_blank())
  
ggsave(
  filename = "graphs/deweather_importance.png",
  plot = imp_plot,
  width = 7,
  height = 5,
  dpi = 300
)