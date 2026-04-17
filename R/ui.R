library(shiny)
library(leaflet)

app_ui <- fluidPage(
  titlePanel("Santa Cruz Wave Buoys"),
  leafletOutput("map", height = 500),
  #verbatimTextOutput("debug_click"),
  plotOutput("spectrum_plot", height = 350)
)
