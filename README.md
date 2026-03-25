# shp-deweathering
SHP Project - Deweathering NO2 in Cambridge 

This project uses the deweather package from OpenAir in R to remove meteorological and long term changes in the data. 

The data used can be found in the "Data" folder. It includes the rural and urban data, where they are split into NO2, traffic counts and meteorological conditions. 

The packages used are:
- tidyverse
- lubridate
- deweather
- patchwork

The code documents are used in the following order:
- map.R
  - Generates a map of the area where the data sites are
- combining_data.R
  - This combines all the datasets for the urban and rural site since they come from different sources
- raw_graphs.R
  - This makes exploratory plots of the data before making any changes to it
- met_plotting.R
  - This is again exploratory plots with the meteorological data
- deweather_no2_covid_only.R
  - This document is used to deweather the urban data, it also makes a plot of the importance of variables
