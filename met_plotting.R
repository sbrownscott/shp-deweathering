# Meteorological data plotting
# Input:
#   - cambridge_combined_met_no2_traffic.csv (for 2014–2020 meteorology)
# Output:
#   - graphs/2020_monthly_temperature_anomalies.png
#   - graphs/2020_monthly_wind_speed_anomalies.png
#   - graphs/2020_monthly_wind_direction_anomalies.png
#   - graphs/no2_pollution_rose_pre_vs_covid.png
#   - graphs/met_timeseries_2020.png


# Load libraries
library(tidyverse)
library(lubridate)
library(openair)
library(patchwork)

# Load and prepare data
data <- read_csv("combined_data/cambridge_combined_met_no2_traffic.csv") %>%
  mutate(
    date = dmy(date) + hms(time),
    temp = as.numeric(temp),
    no2 = as.numeric(no2),
    ws = as.numeric(ws),
    wd = as.numeric(wd),
    year = year(date),
    week = floor_date(date, "week")
  )

# Defining monthly temperature anomalies
monthly_temp <- data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    temp_monthly = mean(temp, na.rm = TRUE),
    .groups = "drop"
  )

monthly_temp <- monthly_temp %>%
  mutate(
    year = year(month),
    period = if_else(year == 2020, "2020", "Other years")
  )

monthly_temp <- monthly_temp %>%
  mutate(
    year = year(month),
    month_number = month(month)
  )

# Calculate climatology (2015–2019) and anomalies for 2020
climatology <- monthly_temp %>%
  filter(year >= 2015 & year <= 2019) %>%
  group_by(month_number) %>%
  summarise(
    clim_temp = mean(temp_monthly, na.rm = TRUE),
    .groups = "drop"
  )

monthly_temp_anom <- monthly_temp %>%
  left_join(climatology, by = "month_number") %>%
  mutate(
    # Calculate anomaly as the difference between monthly temp and climatology
    temp_anomaly = temp_monthly - clim_temp
  )

# Defining monthly wind speed anomalies
monthly_ws <- data %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(
    ws_monthly = mean(ws, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    year = year(month),
    month_number = month(month)
  )

ws_climatology <- monthly_ws %>%
  filter(year >= 2015 & year <= 2019) %>%
  group_by(month_number) %>%
  summarise(
    clim_ws = mean(ws_monthly, na.rm = TRUE),
    .groups = "drop"
  )

monthly_ws_anom <- monthly_ws %>%
  left_join(ws_climatology, by = "month_number") %>%
  mutate(
    ws_anomaly = ws_monthly - clim_ws
  )

# Plot of monthly temperature anomalies in 2020
ggplot(
  monthly_temp_anom %>% 
    filter(year == 2020),
  aes(x = factor(month_number, 
                 levels = 1:12,
                 labels = month.abb), 
      y = temp_anomaly,
      fill = temp_anomaly > 0)
) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#ff0000", 
                               "FALSE" = "#008cff")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(
    aes(label = round(temp_anomaly, 2),
        vjust = ifelse(temp_anomaly > 0, -0.5, 1.5)),
    size = 4
  ) + 
  labs(
    x = "Month",
    y = "Temperature Anomaly (°C)",
    #title = "Monthly Temperature Anomalies in 2020",
    #subtitle = "Relative to 2015–2019 Climatology"
  ) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme(legend.position = "none")
ggsave(
  "graphs/2020_monthly_temperature_anomalies.png",
  width = 10,
  height = 6,
  dpi = 300
)

# Plot of monthly wind speed anomalies in 2020
ggplot(
  monthly_ws_anom %>% 
    filter(year == 2020),
  aes(x = factor(month_number,
                 levels = 1:12,
                 labels = month.abb),
      y = ws_anomaly,
      fill = ws_anomaly > 0)
) +
  geom_col() +
  scale_fill_manual(values = c("TRUE" = "#ff0000",   # windier than normal
                               "FALSE" = "#008cff")) + # calmer than normal
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_text(
    aes(label = round(ws_anomaly, 2),
        vjust = ifelse(ws_anomaly > 0, -0.5, 1.5)),
    size = 4
  ) +
  labs(
    x = "Month",
    y = "Wind Speed Anomaly (m/s)",
    #title = "Monthly Wind Speed Anomalies in 2020",
    #subtitle = "Relative to 2015–2019 Climatology"
  ) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme(legend.position = "none")

ggsave(
  "graphs/2020_monthly_wind_speed_anomalies.png",
  width = 10,
  height = 6,
  dpi = 300
)

# Sorting the data for 2020 anomalies by month number
anom_2020 <- monthly_temp_anom %>%
  filter(year == 2020) %>%
  select(month_number, temp_anomaly) %>%
  left_join(
    monthly_ws_anom %>%
      filter(year == 2020) %>%
      select(month_number, ws_anomaly),
    by = "month_number"
  ) %>%
  mutate(
    Month = month.abb[month_number]
  ) %>%
  select(Month, temp_anomaly, ws_anomaly)

anom_2020 %>%
  mutate(
    temp_anomaly = round(temp_anomaly, 2),
    ws_anomaly   = round(ws_anomaly, 2)
  )

# Pollution rose comparing pre-lockdown (2015–2019) vs 2020
data_plot <- data %>%
  filter(month(date) %in% 1:5) %>%   # Jan–May only
  mutate(
    period = case_when(
      year(date) == 2020 ~ "2020",
      year(date) >= 2015 & year(date) <= 2019 ~ "Climatology",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period), !is.na(wd), !is.na(no2)) %>%
  mutate(
    period = factor(period, levels = c("Climatology", "2020"))
  )

png("graphs/no2_pollution_rose_pre_vs_covid.png",
    width = 1200,
    height = 1000,
    res = 150)

# Create pollution rose plot comparing 2020 vs climatology
pollutionRose(
  data_plot,
  pollutant = "no2",
  statistic = "mean",
  type = "period"
)

dev.off()

# Time series of meteorological variables in 2020
df_met_daily <- data %>%
  filter(date >= as.Date("2020-01-01"),
         date <= as.Date("2020-05-31")) %>%
  mutate(date_day = as.Date(date)) %>%
  group_by(date_day) %>%
  summarise(
    temp = mean(temp, na.rm = TRUE),
    ws = mean(ws, na.rm = TRUE),
    .groups = "drop"
  )

# Plot of daily temperature in 2020
p_temp <- ggplot(df_met_daily, aes(x = date_day, y = temp)) +
  geom_line(alpha = 0.5, colour = "firebrick") +
  geom_smooth(method = "loess", span = 0.2, colour = "darkred", se = FALSE) +
  geom_vline(xintercept = as.Date("2020-03-23"),
             linetype = "dashed", colour = "grey35") +
  labs(
    x = "Date",
    y = "Temperature (°C)"
  ) +
  theme_minimal()

# Plot of daily wind speed in 2020
p_ws <- ggplot(df_met_daily, aes(x = date_day, y = ws)) +
  geom_line(alpha = 0.5, colour = "steelblue") +
  geom_smooth(method = "loess", span = 0.2, colour = "blue", se = FALSE) +
  geom_vline(xintercept = as.Date("2020-03-23"),
             linetype = "dashed", colour = "grey35") +
  labs(
    x = "Date",
    y = "Wind speed (m/s)"
  ) +
  theme_minimal()

ggsave(
  "graphs/met_timeseries_2020.png",
  p_temp / p_ws,
  width = 8,
  height = 6,
  dpi = 300
)

print("End")