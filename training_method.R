# Deweathering NO₂ at Cambridge: Training & Validation
# Inputs:
#   - combined_data/cambridge_combined_met_no2_traffic.csv
# Outputs:
#   - graphs/observed_vs_predicted_2019_baseline.png
#   - graphs/time_series_2019_baseline.png
#   - graphs/residuals_time_series_2019_baseline.png

# Load libraries
library(tidyverse)
library(lubridate)
library(deweather)

# Read & Clean Data

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
  mutate(
    ws = as.numeric(if_else(ws == "No data", NA_character_, ws)),
    wd = as.numeric(if_else(wd == "No data", NA_character_, wd)),
    temp = as.numeric(if_else(temp == "No data", NA_character_, temp)),
    time_orig = time,
    time = if_else(time == "24:00:00", "00:00:00", time),
    datetime = dmy(date) + hms(time),
    datetime = if_else(time_orig == "24:00:00",
                       datetime + days(1),
                       datetime),
    air_temp = temp,
    hour = factor(hour(datetime), levels = 0:23),
    weekday = factor(wday(datetime, label = TRUE)),
    trend = as.numeric(difftime(datetime,
                                min(datetime, na.rm = TRUE),
                                units = "days")),
    wd_sin = sin(wd * pi / 180),
    wd_cos = cos(wd * pi / 180)
  ) %>%
  drop_na(datetime, no2, ws, wd, air_temp)

# Training Data 
train_data <- cambridge %>%
  filter(year(datetime) %in% 2014:2018)

# Test Data
test_data <- cambridge %>%
  filter(year(datetime) == 2019)

# Build deweathering model
dw_model_base <- build_dw_model(
  data = train_data,
  pollutant = "no2",
  vars = c("trend","ws","wd_sin","wd_cos","hour","weekday","air_temp"),
  engine = "xgboost",
  trees = 300,
  tree_depth = 6,
  learn_rate = 0.05,
  .date = "datetime"
)

# Simulate deweathered NO2 for test data
test_pred_base <- simulate_dw_met(
  dw_model_base,
  newdata = test_data,
  n = 1
) %>%
  rename(no2_pred = no2) %>%
  bind_cols(
    test_data %>%
      select(datetime, no2) %>%
      rename(no2_obs = no2)
  )

# Validation: Observed vs Predicted
test_pred_daily <- test_pred_base %>%
  mutate(date = as.Date(datetime)) %>%
  group_by(date) %>%
  summarise(
    no2_obs = mean(no2_obs, na.rm = TRUE),
    no2_pred = mean(no2_pred, na.rm = TRUE)
  ) %>%
  ungroup()

test_pred_daily <- test_pred_daily %>%
  mutate(residual = no2_obs - no2_pred)

fit_line <- lm(no2_pred ~ no2_obs, data = test_pred_daily)

fit_coef <- coef(fit_line)

equation_label <- sprintf(
  "y = %.2f + %.2fx",
  fit_coef[1],
  fit_coef[2]
)

print(paste("Linear fit:", equation_label))

# Observed vs Predicted Plot
validation_plot <- ggplot(test_pred_daily,
                          aes(x = no2_obs, y = no2_pred)) +
  
  geom_point(alpha = 0.5) +
  
  geom_abline(aes(linetype = "1:1 Line"),
              intercept = 0, slope = 1,
              colour = "black") +
  
  geom_smooth(aes(linetype = "Linear Fit"),
              method = "lm",
              se = FALSE,
              colour = "red") +
  
  scale_linetype_manual(
    name = "Lines",
    values = c("1:1 Line" = "dashed",
               "Linear Fit" = "solid")
  ) +
  
  labs(
    x = "Observed Daily Mean NO₂ (µg m⁻³)",
    y = "Predicted Daily Mean NO₂ (µg m⁻³)"
  ) +
  
  theme_minimal() +
  
  # Force axes to start at 0
  expand_limits(x = 0, y = 0)
  
validation_plot

ggsave("graphs/observed_vs_predicted_2019_baseline.png",
       validation_plot,
       width = 7, height = 5, dpi = 300)


time_series_plot <- ggplot(test_pred_daily, aes(x = date)) +
  
  geom_line(aes(y = no2_obs, colour = "Observed"), alpha = 0.3) +
  geom_line(aes(y = no2_pred, colour = "Predicted"), alpha = 0.3) +
  
  geom_smooth(aes(y = no2_obs, colour = "Observed"),
              method = "loess", se = FALSE, span = 0.4) +
  
  geom_smooth(aes(y = no2_pred, colour = "Predicted"),
              method = "loess", se = FALSE, span = 0.4) +
  
  scale_colour_manual(values = c("Observed" = "blue", "Predicted" = "red")) +
  
  labs(
    x = "Date",
    y = "Daily Mean NO₂ (µg m⁻³)",
    colour = "Legend"
  ) +

  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  
  theme_minimal()

time_series_plot

ggsave("graphs/time_series_2019_baseline.png",
       time_series_plot,
       width = 10, height = 5, dpi = 300)

residual_time_plot <- ggplot(test_pred_daily, aes(x = date, y = residual)) +
  
  geom_line(alpha = 0.6) +
  
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "black") +
  
  geom_smooth(method = "loess",
              se = FALSE,
              colour = "red",
              span = 0.2) +
  
  labs(
    x = "Date",
    y = "Residual (Observed − Predicted NO₂)"
  ) +
  
  theme_minimal()

residual_time_plot

ggsave("graphs/residuals_time_series_2019_baseline.png",
       residual_time_plot,
       width = 10, height = 5, dpi = 300)