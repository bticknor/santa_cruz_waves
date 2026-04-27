library(ggplot2)

plot_tide <- function(tide_data) {
  curve <- tide_data$curve
  hilo  <- tide_data$hilo
  now_utc <- as.POSIXct(Sys.time(), tz = "UTC")

  to_pacific <- function(t) { attr(t, "tzone") <- "America/Los_Angeles"; t }

  p <- ggplot()

  if (!is.null(curve) && nrow(curve) > 0) {
    c2 <- curve; c2$time <- to_pacific(c2$time)
    p <- p +
      geom_area(data = c2, aes(x = time, y = water_level_ft),
                fill = "#2980b9", alpha = 0.20) +
      geom_line(data = c2, aes(x = time, y = water_level_ft),
                color = "#2980b9", linewidth = 1.0)
  }

  if (!is.null(hilo) && nrow(hilo) > 0) {
    h2 <- hilo; h2$time <- to_pacific(h2$time)
    h2$label <- sprintf("%s\n%.1f ft", format(h2$time, "%-I:%M %p"), h2$water_level_ft)
    high <- h2[h2$type == "H", ]
    low  <- h2[h2$type == "L", ]

    if (nrow(high) > 0)
      p <- p +
        geom_point(data = high, aes(x = time, y = water_level_ft),
                   color = "#c0392b", size = 2.5) +
        geom_text(data = high, aes(x = time, y = water_level_ft, label = label),
                  vjust = -0.4, size = 2.8, color = "#c0392b", lineheight = 0.9)

    if (nrow(low) > 0)
      p <- p +
        geom_point(data = low, aes(x = time, y = water_level_ft),
                   color = "#27ae60", size = 2.5) +
        geom_text(data = low, aes(x = time, y = water_level_ft, label = label),
                  vjust = 1.4, size = 2.8, color = "#27ae60", lineheight = 0.9)
  }

  now_local <- to_pacific(now_utc)

  p +
    geom_vline(xintercept = as.numeric(now_local),
               color = "#e74c3c", linetype = "dashed", linewidth = 0.8, alpha = 0.7) +
    geom_hline(yintercept = 0, color = "gray70", linetype = "dotted", linewidth = 0.4) +
    scale_x_datetime(date_labels = "%-I %p", date_breaks = "1 hour") +
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.20))) +
    labs(
      title = "Santa Cruz Tide Predictions",
      x     = NULL,
      y     = "Water Level (ft, MLLW)"
    ) +
    theme_minimal() +
    theme(
      plot.title       = element_text(size = 11),
      axis.text.x      = element_text(size = 8),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90"),
      plot.margin      = margin(10, 20, 5, 10)
    )
}
