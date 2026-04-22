library(shiny)
library(leaflet)

source("R/plot_spectrum.R")
source("R/plot_wind_layer.R")


buoys <- data.frame(
  id = c("46012", "46236", "46042", "46239"),
  name = c("NDBC 46012", "NDBC 46236", "NDBC 46042", "NBDC 46239"),
  lat  = c(37.356, 36.760, 36.787, 36.342),
  lng  = c(-122.881, -121.950, -122.408, -122.110),
  stringsAsFactors = FALSE
)

# =======================================================================

app_server <- function(input, output, session) {
  selected_buoy <- reactiveVal(NULL)

  spectrum_cache <- reactivePoll(
    intervalMillis = 5 * 60 * 1000,
    session = session,
    checkFunc = function() {
      files    <- file.path("data/processed", paste0(buoys$id, ".data_spec.rds"))
      existing <- files[file.exists(files)]
      if (length(existing) == 0) return(NA)
      max(file.mtime(existing))
    },
    valueFunc = function() {
      setNames(
        lapply(buoys$id, function(id) {
          f <- file.path("data/processed", paste0(id, ".data_spec.rds"))
          if (!file.exists(f)) return(NULL)
          tryCatch(readRDS(f), error = function(e) NULL)
        }),
        buoys$id
      )
    }
  )

  wind_df <- reactive({
    wind_file <- "data/raw/wind_latest.rds"
    if (file.exists(wind_file)) readRDS(wind_file) else NULL
  })

  output$map <- renderLeaflet({
    m <- leaflet(buoys) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      fitBounds(
        lng1 = -123.2, lat1 = 36.0,
        lng2 = -121.6, lat2 = 37.6
      )

    m <- add_wind_layer(m, wind_df())
    m <- add_wind_arrows(m, wind_df())
    m <- add_wind_legend(m)

    m %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        layerId = ~id,
        label = ~name,
        radius = 8
      ) %>%
      addLayersControl(
        overlayGroups = c("Wind"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  observeEvent(input$map_marker_click, {
    print(input$map_marker_click)
    selected_buoy(input$map_marker_click$id)
  })

  output$debug_click <- renderPrint({
    input$map_marker_click
  })

  output$spectrum_plot <- renderPlot({
    req(selected_buoy())

    buoy_id <- selected_buoy()

    validate(
      need(!is.null(spectrum_cache()[[buoy_id]]), paste("No spectrum data found for station", buoy_id))
    )

    plot_spectrum_with_direction(
      spectrum_data = spectrum_cache()[[buoy_id]],
      station_id    = buoy_id
    )
  })
}
