## <- <- <- Add Libs
library(shiny)
#library(shinydashboard)
library(leaflet)
library(tidyverse)

## <- <- <- Add helper scripts
source("functions.R")

## <- <- <- Ye olde Parameter-Bay
dataDir <- "data/"

pParam <- data.frame(
  dashArray = "6", 
  smoothFactor = 3,
  color="#000000", 
  opacity = 0.7,
  fillOpacity = 0.7
)

labelOptions <- labelOptions(
  textsize = "20px"
)

## <- <- <- Load Data and other important stuff
# Data of Interest
load(paste0(dataDir,"jobs_per_region_suggestions.RData"))
# Map Data
map_data_deu <- geojsonio::geojson_read(paste0(dataDir,"DEU.geojson"), what="sp")
map_data_ita <- geojsonio::geojson_read(paste0(dataDir,"ITA.geojson"), what="sp")
map_data_irl <- geojsonio::geojson_read(paste0(dataDir,"IRL.geojson"), what="sp")
map_data_cze <- geojsonio::geojson_read(paste0(dataDir,"CZE.geojson"), what="sp")
map_data_gbr <- geojsonio::geojson_read(paste0(dataDir,"GBR.geojson"), what="sp")

## <- <- <- TEST AREA
#test <- geojsonio::geojson_read("data/custom.geo.json", what="sp")

## <- <- <- Define UI <- <- <- <- <- <- <- 
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("skillMap", width = "100%", height = "100%"),
  
  absolutePanel(
    style = "border: 3px solid black; 
              padding:10px;     
              border-radius: 25px;
              background: #FFF;", 
    top = 10, right = 10, width = "30%", draggable = FALSE,
    plotOutput("top5"),
    p(),
    selectInput("6thbar", "Additional Field", unique(jobs_per_region_suggestions$DES_OCCUP_L3_NAME))
  )
)

## <- <- <- Define Server <- <- <- <- <- 
server <- function(input, output) {

  
  output$skillMap <- renderLeaflet({
    leaflet() %>%
      setView(lng= 55, lat = 49.480617, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      #addPolygons(data = map_data)
      addPolygons(data=map_data_deu, label = "Germany", fillColor = "yellow",labelOptions = labelOptions, dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity) %>%
      addPolygons(data=map_data_cze, fillColor = "blue", dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity) %>%
      addPolygons(data=map_data_gbr, fillColor = "green", dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity) %>%
      addPolygons(data=map_data_irl, fillColor = "red", dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity) %>%
      addPolygons(data=map_data_ita, fillColor = "purple", dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity)
  })
  
  output$top5 <- renderPlot({
    ggplot(data = mpg, aes(class)) + 
      geom_bar()
  })
  
}

## <- <- <- <- Render App
shinyApp(ui, server)

