library(shiny)
library(leaflet)
library(ggplot2)
library(pracma)

plot_spectrum <- function(file, station_id = NULL) {
  lines <- readLines(file)
  data_lines <- lines[!grepl("^#", lines)]

  latest_line <- data_lines[1]
  parts <- scan(text = latest_line, what = character(), quiet = TRUE)

  yr <- as.integer(parts[1])
  mo <- as.integer(parts[2])
  dy <- as.integer(parts[3])
  hr <- as.integer(parts[4])
  mi <- as.integer(parts[5])
  sep_freq <- as.numeric(parts[6])

  timestamp_utc <- as.POSIXct(
    sprintf("%04d-%02d-%02d %02d:%02d", yr, mo, dy, hr, mi),
    format = "%Y-%m-%d %H:%M",
    tz = "UTC"
  )

  spec_parts <- parts[-(1:6)]

  energy_vals <- spec_parts[seq(1, length(spec_parts), by = 2)]
  freq_labels <- spec_parts[seq(2, length(spec_parts), by = 2)]

  energy <- as.numeric(energy_vals)
  freq_hz <- as.numeric(gsub("[()]", "", freq_labels))

  df <- data.frame(
    timestamp_utc = timestamp_utc,
    frequency_hz = freq_hz,
    period_s = 1 / freq_hz,
    energy = energy
  )

  df <- df[
    is.finite(df$frequency_hz) &
      is.finite(df$period_s) &
      is.finite(df$energy) &
      df$period_s >= 3 &
      df$period_s <= 25,
  ]

  df <- df[order(df$period_s), ]

  smooth_k <- 2
  smooth_vals <- stats::filter(df$energy, rep(1 / smooth_k, smooth_k), sides = 2)
  df$energy_smooth <- as.numeric(smooth_vals)
  df$energy_smooth[is.na(df$energy_smooth)] <- df$energy[is.na(df$energy_smooth)]

  peaks <- findpeaks(
    df$energy_smooth,
    nups = 1,
    ndowns = 1,
    minpeakheight = max(df$energy_smooth, na.rm = TRUE) * 0.12
  )

  if (is.null(peaks)) {
    peak_df <- data.frame()
  } else {
    peak_idx <- peaks[, 2]
    peak_df <- df[peak_idx, c("period_s", "energy", "energy_smooth")]
    peak_df <- peak_df[order(-peak_df$energy_smooth), ]
  }

  ggplot(df, aes(x = period_s)) +
    geom_line(aes(y = energy), linewidth = 0.7, alpha = 0.5, color = "gray40") +
    geom_line(aes(y = energy_smooth), linewidth = 1.2, color = "blue") +
    geom_point(
      data = peak_df,
      aes(y = energy_smooth),
      size = 2.5,
      color = "red"
    ) +
    geom_text(
      data = peak_df,
      aes(y = energy_smooth, label = sprintf("%.1fs", period_s)),
      nudge_y = 0.05 * max(df$energy, na.rm = TRUE),
      size = 3
    ) +
    labs(
      title = paste(
        station_id %||% "Station",
        "Swell Spectrum",
        format(timestamp_utc, tz = "UTC")
      ),
      x = "Period (s)",
      y = expression(m^2 / Hz)
    ) +
    theme_minimal()
}

`%||%` <- function(a, b) {
  if (!is.null(a)) a else b
}

buoys <- data.frame(
  id = c("46012", "46236", "46092", "46042", "46239"),
  name = c("NDBC 46012", "NDBC 46236", "NDBC 46092", "NDBC 46042", "NBDC 46239"),
  lat  = c(37.356, 36.760, 36.751, 36.787, 36.342),
  lng  = c(-122.881, -121.950, -122.029, -122.408, -122.110),
  stringsAsFactors = FALSE
)


app_server <- function(input, output, session) {
  selected_buoy <- reactiveVal(NULL)

  output$map <- renderLeaflet({
    leaflet(buoys) %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      setView(lng = -122.2, lat = 36.9, zoom = 9) %>%
      addCircleMarkers(
        lng = ~lng,
        lat = ~lat,
        layerId = ~id,
        label = ~name,
        radius = 8
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
    file <- paste0("data/raw/", buoy_id, ".data_spec")

    validate(
      need(file.exists(file), paste("No spectrum file found for station", buoy_id))
    )

    plot_spectrum(file, station_id = buoy_id)
  })
}

