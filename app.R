#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mctroubledproperties)
library(leaflet)
library(leafletwrappers)
library(sf)
library(tidyverse)
library(bslib)

mc_bound <- read_rds("./data/mc_bound.rds")
places_mc <- st_read("./data/places_mc.geojson")
mc_troubledprops <- read_rds("./data/mc_prop_data.rds")

tp_cities <- mc_troubledprops$city %>% str_to_title() %>% unique() %>% sort()
tp_props <- mc_troubledprops$communityname %>% str_to_title() %>% unique() %>% sort()

# Define UI for application that draws a histogram
ui <- fluidPage(

  # bslib::bs_theme_preview(),

  theme = bslib::bs_theme(bootswatch = "yeti"),

  shiny::fluidRow(
    h1("Montgomery County Troubled Properties Map"),

    br(),

    br(),

    tags$p(strong("Background:"), "This app allows you to view an interactive leaflet map of properties inspected by Montgomery County. The data the map is built on can be found ", tags$a("here.", href = "https://data.montgomerycountymd.gov/Consumer-Housing/Troubled-Properties-Analysis/bw2r-araf"), "Place boundaries come from ", tags$a(href = "https://www.nhgis.org/data", "NHGIS."), "The map has layers for properties with different compliance-ratings, and with property-points scaled to the number of units in the property. Layers for properties with different ratings are colored by the 'severity' index assigned by the county, from 1 (less severe) to 5 (more severe). Click on a layer to turn it on or off. You can click on a property to see more information about the property. You can limit the map to properties in specific cities by using the City-select box, and limit the map to specific properties by using the property-select box. You can add boundaries of places in Montgomery County by using the place box. The app was created by the City of Takoma Park; for questions or concerns, contact Public Administration Specialist Dan Powers at ", tags$a(href = "mailto:danielp@takomaparkmd.gov", "danielp@takomaparkmd.gov.")),

    shiny::column(width = 3, offset = 0.5,
                  shiny::selectInput(inputId = "propselect", "Search by properties", choices = tp_props, multiple = T)),
    column(width = 3, offset = 0.5,
           shiny::selectInput(inputId = "cityselect", "Search by City", choices = tp_cities, multiple = T)),
    column(shiny::selectInput("placebounds", "Include place boundaries?", choices = c("Yes", "No"), multiple = F, selected = "No"), width = 3, offset = 0.5)
  ),

  strong(shiny::textOutput("map")),

  br(),

  leaflet::leafletOutput("tpmap", height = 500)

)

# Define server logic required to draw a histogram
server <- function(input, output) {

  cityfilt <- reactive({
    df <- mc_troubledprops

    if (!is.null(input$propselect)){

      df <- df %>%
        filter(str_to_title(communityname) %in% input$propselect)
    }

    if (!is.null(input$cityselect)){
      df <- df %>%
        filter(str_to_title(city) %in% input$cityselect)
    }

    df
  })

  output$map <- renderText({

    if (nrow(cityfilt()) > 0){
      output <- "Map of Montgomery County Troubled Properties Data"

    }

    else if (nrow(cityfilt()) == 0){
      output <- "Selected properties are not in selected cities; try de-selecting either cities or properties by clicking in the city or property select box and hitting backspace"
    }

    output

  })


  map_df <- reactive({

    grps <- c()

    markers_severity_num <- function(basemap, filter = NULL, sizeadjust = F){

      output <- basemap


      if (is.null(filter) & nrow(cityfilt()) > 0){

        output <- output %>%
          mctroubledproperties::markers_severity_num(cityfilt(), filter = filter, sizeadjust = sizeadjust)

        grp <- case_when(sizeadjust ~ "All properties: sized to units",
                         T ~ "All properties")
        grps <<- c(grps, grp)

      }

      if (!is.null(filter)){

        if (nrow(cityfilt() %>%
                 filter(rating == filter)) > 0){
          output <- output %>%
            mctroubledproperties::markers_severity_num(cityfilt(), filter = filter, sizeadjust = sizeadjust)

          grp <- case_when(sizeadjust ~ paste0(filter, ": sized to units"),
                           T ~ filter)
          grps <<- c(grps, grp)
        }

        else {
          output <- basemap
        }
      }

      output

    }

    markers_severity <- function(basemap, filter = NULL, sizeadjust = F){

      output <- basemap

      if (is.null(filter) & nrow(cityfilt()) > 0){
        output <- output %>%
          mctroubledproperties::markers_severity(cityfilt(), filter = filter, sizeadjust = sizeadjust)

        grp <- case_when(sizeadjust ~ "All properties: sized to units",
                         T ~ "All properties")


        grps <<- c(grps, grp)
      }

      else if (!is.null(filter)){
        if (nrow(cityfilt() %>%
                 filter(rating == filter)) > 0){

          output <- output %>%
            mctroubledproperties::markers_severity(cityfilt(), filter = filter, sizeadjust = sizeadjust)

          grp <- case_when(sizeadjust ~ paste0(filter, ": sized to units"),
                           T ~ filter)
          grps <<- c(grps, grp)
        }

        else{
          output <- basemap
        }
      }

      output
    }

    troubled_props <- leaflet(cityfilt()) %>%
      addTiles(options = tileOptions(opacity = .5)) %>%
      # addProviderTiles(provider = providers$OpenStreetMap) %>%
      markers_severity() %>%
      markers_severity_num(filter = "Compliant") %>%
      markers_severity_num(filter = "At-Risk") %>%
      markers_severity_num(filter = "Troubled") %>%
      markers_severity(filter = "Tbd") %>%
      markers_severity(sizeadjust = T) %>%
      markers_severity_num(filter = "Compliant", sizeadjust = T) %>%
      markers_severity_num(filter = "At-Risk", sizeadjust = T) %>%
      markers_severity_num(filter = "Troubled", sizeadjust = T) %>%
      markers_severity(filter = "Tbd", sizeadjust = T) %>%
      addLayersControl(overlayGroups = grps, position = "topright", options = layersControlOptions(collapsed = F)) %>%
      hideGroup((grps)[-1]) %>%
      addControl(position = "bottomleft", html = "Click on a dot to see more information about the property. Change layers by clicking the checkboxes next to them")

    troubled_props

  })

  output$tpmap <- leaflet::renderLeaflet({

    output <- map_df()

    if (input$placebounds == "Yes"){
      output <- output %>%
        addPolygons(stroke = T, color = "blue", weight = 0.5, opacity = 0.5, fill = F, label = label_output(places_mc, "{name}"), labelOptions = labelOptions(noHide = T, direction = "center", textOnly = T, style = list(`font-weight` = "bold", padding = "1px 1px", textsize = "9px")), data = places_mc)
    }

    output

  })

}

# Run the application
shinyApp(ui = ui, server = server)
