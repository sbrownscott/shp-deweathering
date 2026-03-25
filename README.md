# shp-deweathering
SHP Project - Deweathering NO2 in Cambridge 

This project uses the deweather package from OpenAir in R to remove meteorological and long term changes in the data. 
The goal is to evaluate the urban NO2 trends and compare it to the rural background data. 

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
- deweather_plotting.R
  - This document uses the deweathered data to make traffic and time series plots
- deweather_no2_wickenfen.R
  - This is the same dewethering process, but applied to the rural data. It then plots the time series of this data
- training_method.R
  - Here the data is trained on a different time period (a "normal" period) and then plotted with the actual data, to test the accuracy.
- deweather_offsetting.R
  - The urban increment is calculated here, and then applied to the urban time series and traffic NO2 plot. 

The full inputs and outputs of each document are explained in the individual documents

The plots will save to a folder called "graphs" and the updated data will save to "combined_data". 
