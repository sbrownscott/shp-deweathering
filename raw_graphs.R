# Raw data exploration graphs
# Prepare data and create raw graphs of NO2 vs traffic, 
#   diurnal profiles, and vehicle type changes during lockdown
# Input: 
#   - cambridge_combined_met_no2_traffic.csv
# Output: 
#   - raw_no2_vs_traffic_hourly.png
#   - traffic_no2_covid_2020_smooth.png
#   - diurnal_traffic_pre_vs_lockdown.png
#   - diurnal_traffic_pre_vs_lockdown_all_days.png
#   - diurnal_comparison_combined.png
#   - diurnal_traffic_and_no2_comparison.png
#   - no2_2014_to_jun_2020.png

# Load libraries
library(tidyverse)
library(lubridate)
library(patchwork)
library(openair)

# Load data
df_all <- read_csv("combined_data/cambridge_combined_met_no2_traffic.csv")

# Prepare data
df <- df_all %>%
  mutate(
    # Convert date and time to datetime
    datetime = dmy(date) + hms(time),
    # Calculate total traffic count
    total_traffic = Car + Motorbike + Bus + OGV1 + OGV2 + LGV 
  ) %>%
  filter(
    datetime >= ymd("2020-01-01"),
    datetime <  ymd("2020-07-01")
  )

# Prepare daily vehicle type data
df_daily_veh <- df %>%
  mutate(date_day = as.Date(datetime)) %>%
  group_by(date_day) %>%
  summarise(
    Car        = mean(Car, na.rm = TRUE),
    Motorbike = mean(Motorbike, na.rm = TRUE),
    Bus        = mean(Bus, na.rm = TRUE),
    OGV1       = mean(OGV1, na.rm = TRUE),
    OGV2       = mean(OGV2, na.rm = TRUE),
    LGV        = mean(LGV, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = -date_day,
    names_to = "vehicle_type",
    values_to = "count"
  )

# Define lockdown period
lockdown_start <- ymd("2020-03-23") # (start of first lockdown in England)
lockdown_end   <- ymd("2020-05-13") # (end of first lockdown in England)
plot_start     <- ymd("2020-01-01") # (start of data for 2020)
plot_end       <- ymd("2020-05-31") # (end of May, to show post-lockdown period)
essential_travel <- ymd("2020-03-15") # (date when essential travel advice was announced)

# Raw NO2 vs traffic (hourly)
p_raw_scatter <- ggplot(df, aes(x = total_traffic, y = no2)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "gam", formula = y ~ s(x), colour = "black") +
  labs(
    x = "Total traffic count (veh hr⁻¹)",
    y = expression(NO[2]~(mu*g~m^-3)),
    title = "Raw NO₂ vs traffic count (hourly)"
  ) +
  theme_minimal()
ggsave(
  "graphs/raw_no2_vs_traffic_hourly.png",
  p_raw_scatter,
  width = 7,
  height = 5,
  dpi = 300
)

# Three panel plot with traffic by vehicle type, total traffic, and NO2
df_daily <- df %>%
  mutate(date_day = as.Date(datetime)) %>%
  group_by(date_day) %>%
  summarise(
    no2 = mean(no2, na.rm = TRUE),
    total_traffic = mean(total_traffic, na.rm = TRUE),
    .groups = "drop"
  )

# Daily mean traffic plot
p_traffic <- ggplot(df_daily, aes(x = date_day, y = total_traffic)) +
  geom_line(alpha = 0.5, colour = "steelblue") +
  geom_smooth(method = "loess", span = 0.2, colour = "blue", se = FALSE) +
  geom_vline(xintercept = lockdown_start, linetype = "dashed", colour = "grey35") +
  annotate(
    "text",
    x = lockdown_start,
    y = Inf,
    label = "Lockdown start",
    vjust = 1.5,
    hjust = -0.1,
    colour = "grey25",
    size = 3.5
  ) +
  geom_vline(xintercept = essential_travel, linetype = "dashed", colour = "darkgreen") +
  annotate(
    "text",
    x = essential_travel,
    y = Inf,
    label = "Essential travel advice",
    vjust = 1.5,
    hjust = 1.1,
    colour = "darkgreen",
    size = 3.5
  ) +
  scale_x_date(limits = c(plot_start, plot_end)) +
  labs(
    y = "Mean daily traffic count (veh hr⁻¹)",
    x = "Date",
    #title = "Traffic counts (daily mean)"
  ) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme_minimal()

# Weekly mean NO2 plot
p_no2 <- ggplot(df_daily, aes(x = date_day, y = no2)) +
  geom_line(alpha = 0.5, colour = "firebrick") +
  geom_smooth(method = "loess", span = 0.2, colour = "darkred", se = FALSE) +
  geom_vline(xintercept = lockdown_start, linetype = "dashed", colour = "grey35") +
  annotate(
    "text",
    x = lockdown_start,
    y = Inf,
    label = "Lockdown start",
    vjust = 1.5,
    hjust = -0.1,
    colour = "grey25",
    size = 3.5
  ) +
  geom_vline(xintercept = essential_travel, linetype = "dashed", colour = "darkgreen") +
  annotate(
    "text",
    x = essential_travel,
    y = Inf,
    label = "Essential travel advice",
    vjust = 1.5,
    hjust = 1.1,
    colour = "darkgreen",
    size = 3.5
  ) +
  scale_x_date(limits = c(plot_start, plot_end)) +
  labs(
    y = expression(NO[2]~(mu*g~m^-3)),
    x = "Date",
    #title = "NO₂ concentrations (daily mean, raw)"
  ) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme_minimal()

ggsave(
  "graphs/traffic_no2_covid_2020_smooth.png",
  p_traffic / p_no2,
  width = 8,
  height = 6,
  dpi = 300
)

# Vehicle type change during lockdown
vehicle_change <- df_daily_veh %>%
  mutate(period = case_when(
    date_day < lockdown_start ~ "baseline",
    date_day >= lockdown_start &
      date_day <= lockdown_end ~ "lockdown",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(period)) %>%
  group_by(vehicle_type, period) %>%
  summarise(mean_count = mean(count, na.rm = TRUE),
            .groups = "drop") %>%
  pivot_wider(names_from = period,
              values_from = mean_count) %>%
  mutate(percent_change =
           (lockdown - baseline) / baseline * 100)
vehicle_change

# Diurnal profiles
df_diurnal_split <- df %>%
  mutate(
    hour = hour(datetime),
    wday = wday(datetime, week_start = 1),   # 1 = Monday
    day_type = if_else(wday <= 5, "Weekday", "Weekend"),
    period = case_when(
      datetime < lockdown_start ~ "Pre-COVID",
      datetime >= lockdown_start &
        datetime <= lockdown_end ~ "Lockdown",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(day_type, period, hour) %>%
  summarise(
    mean_traffic = mean(total_traffic, na.rm = TRUE),
    .groups = "drop"
  )

# Graphing raw diurnal profiles
p_weekday <- df_diurnal_split %>%
  filter(day_type == "Weekday") %>%
  ggplot(aes(x = hour,
             y = mean_traffic,
             colour = period)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    x = "Hour of day",
    y = "Mean traffic count (veh hr⁻¹)",
    colour = "Period",
    #title = "Weekday diurnal traffic profile"
  ) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme_minimal()

# Weekend diurnal profile
p_weekend <- df_diurnal_split %>%
  filter(day_type == "Weekend") %>%
  ggplot(aes(x = hour,
             y = mean_traffic,
             colour = period)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    x = "Hour of day",
    y = "Mean traffic count (veh hr⁻¹)",
    colour = "Period",
    #title = "Weekend diurnal traffic profile"
  ) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme_minimal()

# All-days diurnal profile (weekday + weekend combined)
df_diurnal_all_days <- df %>%
  mutate(
    hour = hour(datetime),
    period = case_when(
      datetime < lockdown_start ~ "Pre-COVID",
      datetime >= lockdown_start &
        datetime <= lockdown_end ~ "Lockdown",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period, hour) %>%
  summarise(
    mean_traffic = mean(total_traffic, na.rm = TRUE),
    .groups = "drop"
  )

p_all_days <- ggplot(df_diurnal_all_days,
                     aes(x = hour, y = mean_traffic, colour = period)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    x = "Hour of day",
    y = "Mean traffic count (veh hr⁻¹)",
    colour = "Period",
    #title = "Diurnal traffic profile"
  ) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme_minimal()

ggsave(
  "graphs/diurnal_traffic_pre_vs_lockdown.png",
  p_weekday / p_weekend,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "graphs/diurnal_traffic_pre_vs_lockdown_all_days.png",
  p_all_days,
  width = 7,
  height = 4,
  dpi = 300
)

# Prepare datetime and weekday
df_no <- df_all %>%
  mutate(
    datetime = dmy(date) + hms(time),
    date = dmy(date),
    hour = hour(datetime),
    weekday = wday(date, label = TRUE),
    day_type = if_else(weekday %in% c("Sat", "Sun"), "Weekend", "Weekday")
  )

# Combined diurnal profile (weekday + weekend together)
df_diurnal_combined <- df_no %>%
  mutate(
    period = case_when(
      date >= as.Date("2020-01-01") & date < as.Date("2020-03-22") ~ "Pre-COVID",
      date >= as.Date("2020-03-23") & date <= as.Date("2020-05-31") ~ "COVID",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  group_by(period, hour) %>%
  summarise(
    mean_no2 = mean(no2, na.rm = TRUE),
    .groups = "drop"
  )

# Combined diurnal NO2 profile
p_combined <- df_diurnal_combined %>%
  ggplot(aes(x = hour,
             y = mean_no2,
             colour = period)) +
  geom_line(linewidth = 1.2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    x = "Hour of day",
    y = "Mean NO₂ (µg/m³)",
    colour = "Period",
    #title = "Diurnal NO₂ profile"
  ) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme_minimal()

ggsave(
  "graphs/diurnal_comparison_combined.png",
  p_combined,
  width = 10,
  height = 4,
  dpi = 300
)

# traffic and no2 diurnal saved
ggsave(
  "graphs/diurnal_traffic_and_no2_comparison.png",
  p_all_days / p_combined,
  width = 10,
  height = 8,
  dpi = 300
)

# Full NO2 trend up to end of June 2020

df_jan_jun <- df_all %>%
  mutate(datetime = dmy(date) + hms(time)) %>%
  filter(datetime <= ymd("2020-06-30"))  # keep data up to end of June

# Plot NO2 trend
p_2014_jun_no2 <- ggplot(df_jan_jun, aes(x = datetime, y = no2)) +
  geom_line(colour = "firebrick", alpha = 0.6) +
  geom_smooth(method = "loess", span = 0.2, colour = "darkred", se = FALSE) +
  labs(
    #title = "NO₂ Time Series (Start of Data – End of June 2020)",
    x = "Date",
    y = expression(NO[2]~(mu*g~m^-3))
  ) +
  theme_minimal() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Save the plot
ggsave(
  "graphs/no2_2014_to_jun_2020.png",
  p_2014_jun_no2,
  width = 10,
  height = 5,
  dpi = 300
)

# Unused Graphs below - just for exploration, not included in final report
# Wind speed versus NO2

# Ensure wind speed is numeric
df <- df %>%
  mutate(ws = as.numeric(ws))

# Raw NO2 vs wind speed (hourly)
p_no2_ws <- ggplot(df, aes(x = ws, y = no2)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = expression("Wind speed (m s"^-1*")"),
    y = expression(NO[2]~(mu*g~m^-3)),
    #title = "Raw NO₂ vs wind speed (hourly)"
  ) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme_minimal()

ggsave(
  "graphs/raw_no2_vs_wind_speed_hourly.png",
  p_no2_ws,
  width = 7,
  height = 5,
  dpi = 300
)

df <- df %>%
  mutate(
    temp = na_if(temp, "No data"),
    temp = as.numeric(temp)
  )

p_no2_temp <- ggplot(df, aes(x = temp, y = no2)) +
  geom_point(alpha = 0.3, size = 1) +
  geom_smooth(method = "lm", se = TRUE, colour = "black") +
  labs(
    x = expression("Air temperature (°C)"),
    y = expression(NO[2]~(mu*g~m^-3)),
    #title = "Raw NO₂ vs air temperature (hourly)"
  ) +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  theme_minimal()

ggsave(
  "graphs/raw_no2_vs_air_temp_hourly.png",
  p_no2_temp,
  width = 7,
  height = 5,
  dpi = 300
)

print("End")