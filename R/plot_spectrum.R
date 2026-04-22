library(ggplot2)
library(dplyr)

plot_spectrum_with_direction <- function(spectrum_data,
                                         station_id = NULL,
                                         arrow_spacing_s = 2) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  spec_dat      <- spectrum_data$spec_dat
  peak_df       <- spectrum_data$peak_df
  timestamp_utc <- spectrum_data$timestamp_utc

  arrow_df <- spec_dat |>
    filter(is.finite(direction_deg)) |>
    arrange(period_s)

  if (nrow(arrow_df) > 0) {
    arrow_periods <- seq(
      ceiling(min(spec_dat$period_s, na.rm = TRUE)),
      floor(max(spec_dat$period_s, na.rm = TRUE)),
      by = arrow_spacing_s
    )

    idx <- sapply(arrow_periods, function(tp) {
      which.min(abs(arrow_df$period_s - tp))
    })

    arrow_df <- arrow_df[idx, , drop = FALSE] |>
      distinct(period_s, .keep_all = TRUE) |>
      arrange(period_s)

    ymax   <- max(spec_dat$energy_smooth, na.rm = TRUE)
    xspan  <- diff(range(spec_dat$period_s, na.rm = TRUE))
    y_arrow <- -0.10 * ymax

    dir_toward <- (arrow_df$direction_deg + 180) %% 360
    angle_rad  <- (90 - dir_toward) * pi / 180

    arrow_df <- arrow_df |>
      mutate(
        y_arrow = y_arrow,
        xend    = period_s + 0.05 * xspan * cos(angle_rad),
        yend    = y_arrow  + 0.07 * ymax  * sin(angle_rad)
      )
  }

  ymax <- max(spec_dat$energy_smooth, na.rm = TRUE)

  p <- ggplot(spec_dat, aes(x = period_s)) +
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
      aes(y = energy_smooth, label = label),
      nudge_y = 0.10 * max(spec_dat$energy, na.rm = TRUE),
      size = 4.5
    ) +
    labs(
      title = paste(
        station_id %||% "Station",
        "Swell Spectrum",
        format(timestamp_utc, tz = "America/Los_Angeles", usetz = TRUE)
      ),
      x = "Period (s)",
      y = expression("Energy (" * m^2 / Hz * ")")
    ) +
    coord_cartesian(
      ylim = c(-0.16 * ymax, 1.12 * ymax),
      clip = "off"
    ) +
    theme_minimal() +
    theme(plot.margin = margin(10, 20, 30, 20))

  if (nrow(arrow_df) > 0) {
    p <- p +
      geom_segment(
        data = arrow_df,
        aes(x = period_s, y = y_arrow, xend = xend, yend = yend),
        inherit.aes = FALSE,
        arrow = grid::arrow(length = grid::unit(0.16, "inches")),
        linewidth = 0.7,
        color = "darkgreen"
      )
  }

  p
}
