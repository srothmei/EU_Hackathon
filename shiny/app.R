## <- <- <- Add Libs
library(shiny)
#library(shinydashboard)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(stringr)

## <- <- <- Add helper scripts
source("functions.R")

## <- <- <- Ye olde Parameter-Bay
dataDir <- "data/"

panelStyle <- "border: 3px solid black; 
                padding:10px;     
                border-radius: 25px;
                background: #FFF;
                -webkit-box-shadow: 3px 3px 5px 6px #ccc;
                -moz-box-shadow:    3px 3px 5px 6px #ccc;
                box-shadow:         3px 3px 5px 6px #ccc;"

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

highlights <-  highlightOptions(
  weight = 5,
  color = "#666",
  dashArray = "",
  fillOpacity = 0.7,
  bringToFront = TRUE)

## <- <- <- Load Data and other important stuff
# Data of Interest
load(paste0(dataDir,"jobs_per_region_suggestions.RData"))
# Map Data
#map_data_deu <- geojsonio::geojson_read(paste0(dataDir,"DEU.geojson"), what="sp")
#map_data_ita <- geojsonio::geojson_read(paste0(dataDir,"ITA.geojson"), what="sp")
#map_data_irl <- geojsonio::geojson_read(paste0(dataDir,"IRL.geojson"), what="sp")
#map_data_cze <- geojsonio::geojson_read(paste0(dataDir,"CZE.geojson"), what="sp")
#map_data_gbr <- geojsonio::geojson_read(paste0(dataDir,"GBR.geojson"), what="sp")

## <- <- <- TEST AREA
#test <- geojsonio::geojson_read("data/custom.geo.json", what="sp")

## <- <- <- <- <- t' startin' parameters ########################
regions <- tibble(
    #labels = sort(as.vector(get_regionNames(jobs_per_region_suggestions))),
    labels = c("ČESKÁ REPUBLIKA", "DEUTSCHLAND", "IRELAND", "ITALIA", "UNITED KINGDOM" ),
    colors = brewer.pal(5, "Accent"),
    flag_files = c("flag_cz_png.png","flag_de_png.png","flag_ie_png.png","flag_it_png.png","flag_uk_png.png"),
    map_files = c("CZE.geojson", "DEU.geojson", "IRL.geojson", "ITA.geojson", "GBR.geojson")
)


## <- <- <- Define UI <- <- <- <- <- <- <- 
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # The Background Map in aRRRRRRR
  leafletOutput("skillMap", width = "100%", height = "100%"),
  
  ### DEBUGGING AREA ######
  absolutePanel(
    style= panelStyle, top = 200, left = 50, draggable = TRUE, width = "20%",
    textOutput("debugText")
  ),
  
  ## The Logo
  absolutePanel(
    top = 10, left = 50,
    imageOutput("logo")
  ),
  
  # Define th' country selector
  absolutePanel(
    style = panelStyle,
    bottom = 10, left = 10, width = "50%", draggable = FALSE,
    radioButtons("selectCountry", 
                 "Select your Region for analysis", 
                 regions$labels,
                 inline = TRUE
                 )
  ),
  
  # Ye olde plot panel
  absolutePanel(
    style = panelStyle, 
    top = 10, right = 10, width = "40%", draggable = FALSE,
    plotOutput("top5"),
    div(
      strong("Additional Field:"),
      checkboxInput("addCol",NULL, width = 30)
    ),
    selectInput("sixthbar", NULL, c("",unique(jobs_per_region_suggestions$DES_OCCUP_L3_NAME)), selected = NULL),
    #   flowLayout(
    #     actionButton("addBar","Add"),
    #     actionButton("delBar","Remove"))
    p(),
    plotOutput("top6")
  )
)

## <- <- <- Define Server <- <- <- <- <- 
server <- function(input, output, session) {
  
  ## Logo
  output$logo <- renderImage({
    list(src = "www/Logo_ELMM_001bb_PNG.png", width="400px")
  }, deleteFile = FALSE)
  
  output$skillMap <- renderLeaflet({
    leaflet() %>%
      setView(lng= 55, lat = 49.480617, zoom = 4) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=geojsonio::geojson_read(paste0(dataDir,regions$map_files[1]), what="sp"), group = regions$labels[1], label = regions$labels[1], fillColor = regions$colors[1],labelOptions = labelOptions, dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity, highlight = highlights) %>%
      addPolygons(data=geojsonio::geojson_read(paste0(dataDir,regions$map_files[2]), what="sp"), group = regions$labels[2], label = regions$labels[2], fillColor = regions$colors[2],labelOptions = labelOptions, dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity, highlight = highlights) %>%
      addPolygons(data=geojsonio::geojson_read(paste0(dataDir,regions$map_files[3]), what="sp"), group = regions$labels[3], label = regions$labels[3], fillColor = regions$colors[3],labelOptions = labelOptions, dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity, highlight = highlights) %>%
      addPolygons(data=geojsonio::geojson_read(paste0(dataDir,regions$map_files[4]), what="sp"), group = regions$labels[4], label = regions$labels[4], fillColor = regions$colors[4],labelOptions = labelOptions, dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity, highlight = highlights) %>%
      addPolygons(data=geojsonio::geojson_read(paste0(dataDir,regions$map_files[5]), what="sp"), group = regions$labels[5], label = regions$labels[5], fillColor = regions$colors[5],labelOptions = labelOptions, dashArray= pParam$dashArray, smoothFactor = pParam$smoothFactor, color=pParam$color, opacity = pParam$opacity, fillOpacity = pParam$fillOpacity, highlight = highlights)
  })
  
  ## Reactive Additional value
  sixthBar_reac <- reactive({
    input$sixthbar
  })
  
  ## REACTIVE Plot
  data_interm <- reactive({
    get_top_perOccup(data = jobs_per_region_suggestions, regionName = input$selectCountry, n = 5)
  })
  
  data <- reactive({
    if(input$addCol) {
      return(add_occup(jobs_per_region_suggestions, data_interm(), sixthBar_reac(), input$selectCountry))
    }
    data_interm()
  })

  output$top5 <- renderPlot({
    #Check for default val
    #if(!is.null(input$skillMap_shape_click[[3]])) {
      #data <- get_top_perOccup(data = jobs_per_region_suggestions, regionName = input$selectCountry, n = 5)
      
     # ggplot(data = data(), aes(x = DES_OCCUP_L3_NAME, y = numMissing)) +
      #  geom_bar(stat = "identity", aes(fill = DES_OCCUP_L3_NAME)) +
      #  coord_flip() +
      #  ylab(NULL) + xlab(NULL) +
      #  scale_fill_brewer(palette = "YlGnBu", guide=FALSE) +
      #  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
      #  theme_minimal()
    
    ggplot(data = data(), aes(x = DES_OCCUP_L3_NAME, y = numMissing, width=0.95)) +
      geom_bar(stat = "identity", aes(fill = DES_OCCUP_L3_NAME), width = 0.1) +
      coord_flip() +
      ylab("Worker Supply Gap") + xlab(NULL) +
      scale_fill_brewer(palette = "YlGnBu", guide=FALSE) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
      theme_minimal(base_size = 18)
    
    #}
  })
  
  data_6 <- reactive({
    get_perOccup_ctry(data = jobs_per_region_suggestions, 
                      regionName = input$selectCountry, 
                      occup = sixthBar_reac())
  })
  
  output$top6 <- renderPlot({
    if(sixthBar_reac() != "") {
    #data <- get_perOccup_ctry(data = jobs_per_region_suggestions, regionName = "DEUTSCHLAND", occup = "Contact centre information clerks")
    data <- data_6()
    
    ggplot(data, aes(name_alternative)) +
      geom_bar(data = subset(data, count.up == "positive"), aes(y = numMissing_alternative, fill = name_alternative), stat = "identity", position = "dodge") +
      geom_bar(data = subset(data, count.up == "negative"), aes(y = numMissing_alternative, fill = name_alternative), stat = "identity", position = "dodge") +
      geom_hline(yintercept = 0,colour = "grey90") +
      coord_flip() +
      ylab("Worker Supply Gap") + xlab(NULL) +
      scale_fill_brewer(palette = "YlGnBu", guide=FALSE) +
      scale_x_discrete(labels = function(x) str_wrap(x, width = 40)) +
      theme_minimal(base_size = 18)

    }
  })
  
  output$debugText <- renderText({
    #print(paste("Radio Button:",input$selectCountry," ### ", input$skillMap_shape_click))
    #typeof(input$skillMap_shape_click[[3]])
    # if(is.null(input$skillMap_shape_click[[3]])) {
    #   print("nein")
    # } else {
    #   paste(get_top_perOccup(data = jobs_per_region_suggestions, regionName = input$skillMap_shape_click[[3]], n = 5))
    # }
    paste(sixthBar_reac())
  })
  
  ## RADIO Button Control
  observe({
    x <- input$skillMap_shape_click[[3]]
    updateRadioButtons(session, "selectCountry", selected = x)
  })
  
  ## Add-Box
  # observeEvent(input$addCol,{
  #   if(input$addCol) {
  #     text <- sixthBar_reac()
  #   } else {
  #     text <- "nein"
  #   }
  #   output$debugText <- renderText({print(text)})
  # })
  
  # observeEvent(input$addBar, {
  #   #data <- add_occup(data = jobs_per_region_suggestions, occup_name = input$sixthbar, regionName = input$selectCountry)
  #   #output$debugText <- renderText({print(input$sixthbar)})
  # })
  # ## Remove-Button
  # observeEvent(input$delBar, {
  # 
  # })
 }

## <- <- <- <- Render App
shinyApp(ui, server)

