## LEAFLET
library(leaflet)
library(RColorBrewer)

# Load map data
map_data_ger <- readRDS("data/DEU_adm1.rds")

# Color Array for testing
colorArray <- round(rnorm(16)*10+1)

# Create a continuous palette function
pal <- colorNumeric(
  palette = "RdYlGn",
  domain = colorArray)

m <- leaflet(map_data_ger) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
 # addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolygons(fillColor = pal(colorArray), weight = 2,
              color="#000000", opacity = 1, dashArray = "4",
              fillOpacity = 0.7,
              label = ~NAME_1,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = TRUE)) %>% 
  addLegend(pal = pal, values = colorArray, opacity = 0.7, title = NULL,
                position = "bottomright")
  #addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map