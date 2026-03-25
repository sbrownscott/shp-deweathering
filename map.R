# Create a map of the monitoring sites in Cambridge,
# and a second map showing key influence locations for the city.
# This code uses the leaflet package to create interactive maps,
# and saves them as HTML files.

# Inputs: None
# Outputs: site_map.html, regional_influences_map.html

library(leaflet) # For creating interactive maps
library(htmlwidgets) # For saving the maps as HTML files

# Create a dataframe
sites <- data.frame(
  name = c("Parker Street", "East Road", "Regent Street", "Wicken  Fen"),
  type = c("NO₂", "Traffic", "Meteorological", "Rural Background"),
  lat  = c(52.204608, 52.20407, 52.202370, 52.3020),
  lon  = c(0.125891, 0.13294, 0.124456, 0.2980)
)

# Colour palette
pal <- colorFactor(
  palette = c("purple", "red", "blue", "green"),
  domain = sites$type
)

# Create the map
site_map <- leaflet(sites) %>%
  addTiles() %>%
  addCircleMarkers(
    ~lon, ~lat,
    color = ~pal(type),
    radius = 7,
    fillOpacity = 0.9,
    label = ~name
  ) %>%
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~type,
    title = "Site type",
    opacity = 1
  )

# Save as HTML
saveWidget(site_map, "site_map.html", selfcontained = TRUE)


# Data frame of key influence locations
influences <- data.frame(
  name = c("Cambridge Centre",
           "Wicken Fen (Rural Background)",
           "Marylebone Road (Kerbside, London)"),
  type = c("Urban Centre", "Rural Background", "Urban Kerbside"),
  lat  = c(52.2053, 52.3020, 51.5225),
  lon  = c(0.1218, 0.2980, -0.1546)
)

# Colour palette
pal <- colorFactor(
  palette = c("purple", "green", "red"),
  domain = influences$type
)

# Create map
influence_map <- leaflet(influences) %>%
  addTiles() %>%
  
  # Add site markers
  addCircleMarkers(
    ~lon, ~lat,
    color = ~pal(type),
    radius = 8,
    fillOpacity = 0.9,
    label = ~name
  ) %>%
  
  # Add SW wind arrow (as a polyline)
  addPolylines(
    lng = c(-0.05, 0.1218),
    lat = c(52.10, 52.2053),
    color = "black",
    weight = 3,
    label = "Dominant SW Wind"
  ) %>%
  
  # Add legend
  addLegend(
    position = "bottomright",
    pal = pal,
    values = ~type,
    title = "Site Type",
    opacity = 1
  ) %>%
  
  # Set view to include London + Cambridge
  setView(lng = 0.05, lat = 52.0, zoom = 8)

# Save
saveWidget(influence_map, "regional_influences_map.html", selfcontained = TRUE)