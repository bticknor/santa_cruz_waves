library(shiny)
library(leaflet)

app_ui <- fluidPage(
  titlePanel("Current Santa Cruz Wave Conditions"),
  fluidRow(
    column(
      width = 5,
      plotOutput("spectrum_plot", height = 500)
    ),
    column(
      width = 7,
      leafletOutput("map", height = 800)
    )
  )
)