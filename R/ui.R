library(shiny)
library(leaflet)

app_ui <- fluidPage(
  titlePanel("Current Santa Cruz Wave Conditions"),
  fluidRow(
    column(
      width = 5,
      uiOutput("spectrum_area"),
      plotOutput("tide_plot", height = 250)
    ),
    column(
      width = 7,
      leafletOutput("map", height = 800)
    )
  )
)