knitr::opts_chunk$set(error = TRUE) 
require(mosaic)
require(rgdal)
require(leaflet)
require(shiny)

ui <- fluidPage(
  titlePanel("Canvasback Population"),
  
  sidebarPanel(
    sliderInput("year", "Year",
                min = 2000, max = 2015, value = 2011, step = 1, sep=""), width = 3
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Map 2005", leafletOutput("mymap1", width=600, height=500)),
      tabPanel("Map 2008", leafletOutput("mymap2", width=600, height=500)),
      tabPanel("Map 2012", leafletOutput("mymap3", width=600, height=500)),
      tabPanel("Population by Strata by Year", plotOutput("plot", width=600, height=500)),
      tabPanel("Harvest/AgeRatio/Season by Year", tableOutput("table"))
    ))
)

