# Combining datasets for Cambridge analysis
# This script reads in the raw meteorology, NO2,
#   and traffic datasets for Cambridge,
# combines them into a single dataframe, and exports the combined data to CSV.
# Inputs:
#   - cambridge_roadside_5year.csv (for 2014–2019 meteorology)
#   - cambridge_roadside_covid.csv (for 2020 meteorology)
#   - cambridge_no2_[year].csv (for 2014–2019 NO2)
#   - parker_street_no2.csv (for 2020 NO2)
#   - east_road_traffic.csv (for 2014–2020 traffic)
# Outputs:
#   - cambridge_combined_met_no2_traffic.csv
#   - wickenfen_combined_met_no2.csv

# Load libraries
library(tidyverse)
library(lubridate)

# Cambridge meteorology
cam_5yr <- read_csv("data/cambridge_roadside_5year.csv")
cam_covid <- read_csv("data/cambridge_roadside_covid.csv")

cam_met <- bind_rows(cam_5yr, cam_covid) %>%
  select(date, time, ws, wd, temp)

# Cambridge NO2
years <- 2014:2019

# Loop through each year in years, read the corresponding NO2 file,
# and combine into a single dataframe
cam_no2_list <- lapply(years, function(y) {
  read_csv(paste0("data/cambridge_no2_", y, ".csv")) %>%
    rename(date = `End Date`, time = `End Time`, no2 = NO2) %>%
    select(date, time, no2)
})

cam_no2_2014_2019 <- bind_rows(cam_no2_list)

# Read 2020+ Parker Street NO2
parker_no2 <- read_csv("data/parker_street_no2.csv") %>%
  rename(date = `End Date`, time = `End Time`, no2 = NO2) %>%
  select(date, time, no2)

# Combine all years
parker_no2_all <- bind_rows(cam_no2_2014_2019, parker_no2) %>%
  arrange(date, time) 

# East Road traffic
traffic_raw <- read_csv("data/east_road_traffic.csv")

traffic <- traffic_raw %>%
  group_by(Date, Time) %>%   # combine in + out
  summarise(
    Car        = sum(Car, na.rm = TRUE),
    Motorbike = sum(Motorbike, na.rm = TRUE),
    Bus        = sum(Bus, na.rm = TRUE),
    OGV1       = sum(OGV1, na.rm = TRUE),
    OGV2       = sum(OGV2, na.rm = TRUE),
    LGV        = sum(LGV, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(
    date = Date,
    time = Time
  )

# Combine everything
final_df <- cam_met %>%
  left_join(parker_no2_all, by = c("date", "time")) %>%
  left_join(traffic,    by = c("date", "time"))

# Save to CSV
write_csv(final_df, "combined_data/cambridge_combined_met_no2_traffic.csv")

# Wicken Fen meteorology
wicken_met_5yr <- read_csv("data/wickenfen_5year.csv")
wicken_met_covid <- read_csv("data/wickenfen_covid.csv")
wicken_met <- bind_rows(wicken_met_5yr, wicken_met_covid) %>%
  select(date, time, no2, ws, wd, temp)

# Save Wicken Fen combined meteorology to CSV
write_csv(wicken_met, "combined_data/wickenfen_combined_met_no2.csv")