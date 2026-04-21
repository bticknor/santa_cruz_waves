library(ggplot2)
library(pracma)
library(dplyr)

plot_spectrum_with_direction <- function(spec_file,
                                         swdir_file,
                                         station_id = NULL,
                                         period_min = 3,
                                         period_max = 25,
                                         arrow_spacing_s = 2) {

  `%||%` <- function(a, b) if (!is.null(a)) a else b

  deg_to_compass <- function(deg) {
    dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
    ix <- round((deg %% 360) / 45) %% 8 + 1
    dirs[ix]
  }

  read_ndbc_spec <- function(file) {
    lines <- readLines(file, warn = FALSE)
    data_lines <- lines[!grepl("^#", lines)]

    if (length(data_lines) == 0) stop("No data rows in spec file")

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

    out <- data.frame(
      timestamp_utc = timestamp_utc,
      frequency_hz = as.numeric(gsub("[()]", "", freq_labels)),
      energy = as.numeric(energy_vals),
      sep_freq = sep_freq
    )

    out$period_s <- 1 / out$frequency_hz

    out |>
      filter(
        is.finite(frequency_hz),
        is.finite(period_s),
        is.finite(energy),
        period_s >= period_min,
        period_s <= period_max
      ) |>
      arrange(period_s)
  }

  read_ndbc_swdir <- function(file) {
    lines <- readLines(file, warn = FALSE)
    data_lines <- lines[!grepl("^#", lines)]

    if (length(data_lines) == 0) stop("No data rows in swdir file")

    latest_line <- data_lines[1]
    parts <- scan(text = latest_line, what = character(), quiet = TRUE)

    yr <- as.integer(parts[1])
    mo <- as.integer(parts[2])
    dy <- as.integer(parts[3])
    hr <- as.integer(parts[4])
    mi <- as.integer(parts[5])

    timestamp_utc <- as.POSIXct(
      sprintf("%04d-%02d-%02d %02d:%02d", yr, mo, dy, hr, mi),
      format = "%Y-%m-%d %H:%M",
      tz = "UTC"
    )

    dir_parts <- parts[-(1:5)]
    dir_vals <- dir_parts[seq(1, length(dir_parts), by = 2)]
    freq_labels <- dir_parts[seq(2, length(dir_parts), by = 2)]

    out <- data.frame(
      timestamp_utc = timestamp_utc,
      frequency_hz = as.numeric(gsub("[()]", "", freq_labels)),
      direction_deg = as.numeric(dir_vals)
    )

    out$period_s <- 1 / out$frequency_hz
    out$direction_deg[out$direction_deg >= 999] <- NA_real_

    out |>
      filter(
        is.finite(frequency_hz),
        is.finite(period_s),
        period_s >= period_min,
        period_s <= period_max
      ) |>
      mutate(
        direction_deg = ifelse(direction_deg < 0 | direction_deg > 360,
                               NA_real_,
                               direction_deg)
      ) |>
      arrange(period_s)
  }

  spec_df <- read_ndbc_spec(spec_file)
  dir_df  <- read_ndbc_swdir(swdir_file)

  spec_dat <- spec_df |>
    left_join(
      dir_df |> select(frequency_hz, direction_deg),
      by = "frequency_hz"
    ) |>
    arrange(period_s)

  if (nrow(spec_dat) == 0) stop("No usable spectrum data after filtering")

  # frequency step
  spec_dat$freq_step <- c(diff(spec_dat$frequency_hz), NA_real_)
  fallback_step <- median(abs(diff(spec_dat$frequency_hz)), na.rm = TRUE)
  if (!is.finite(fallback_step)) fallback_step <- 0
  spec_dat$freq_step[is.na(spec_dat$freq_step)] <- fallback_step
  spec_dat$freq_step <- abs(spec_dat$freq_step)

  # smooth spectrum
  smooth_k <- 2
  smooth_vals <- stats::filter(spec_dat$energy, rep(1 / smooth_k, smooth_k), sides = 2)
  spec_dat$energy_smooth <- as.numeric(smooth_vals)
  spec_dat$energy_smooth[is.na(spec_dat$energy_smooth)] <- spec_dat$energy[is.na(spec_dat$energy_smooth)]

  # find peaks
  peaks <- findpeaks(
    spec_dat$energy_smooth,
    nups = 1,
    ndowns = 1,
    minpeakheight = max(spec_dat$energy_smooth, na.rm = TRUE) * 0.12
  )

  if (is.null(peaks)) {
    peak_df <- data.frame(
      period_s = numeric(),
      energy = numeric(),
      energy_smooth = numeric(),
      direction_deg = numeric(),
      freq_step = numeric(),
      m0 = numeric(),
      Hs_peak = numeric(),
      power_raw = numeric(),
      power_pct = numeric(),
      dir_label = character(),
      label = character()
    )
  } else {
    peak_idx <- peaks[, 2]

    peak_df <- spec_dat[peak_idx, c("period_s", "energy", "energy_smooth", "direction_deg")]
    peak_df <- peak_df[order(-peak_df$energy_smooth), ]

    peak_df <- peak_df |>
      left_join(spec_dat |> select(period_s, freq_step), by = "period_s") |>
      mutate(
        m0_raw = energy_smooth * freq_step,
        m0 = ifelse(is.finite(m0_raw), pmax(m0_raw, 0), 0),
        Hs_peak = 4 * sqrt(m0) * 3.28084,  # this is in feet
        power_raw = ifelse(is.finite(energy_smooth), pmax(energy_smooth, 0) * period_s, 0)
      ) |>
      select(-m0_raw)

    total_power <- sum(peak_df$power_raw, na.rm = TRUE)

    peak_df <- peak_df |>
      mutate(
        power_pct = if (total_power > 0) 100 * power_raw / total_power else NA_real_,
        dir_label = ifelse(is.na(direction_deg), "", deg_to_compass(direction_deg)),
        label = ifelse(
          dir_label == "",
          sprintf("%.0f%%\n%.1fs\n%.1fft", power_pct, period_s, Hs_peak),
          sprintf("%.0f%%\n%.1fs %s\n%.1fft", power_pct, period_s, dir_label, Hs_peak)
        )
      )
  }

  # direction arrows
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

    ymax <- max(spec_dat$energy_smooth, na.rm = TRUE)
    xspan <- diff(range(spec_dat$period_s, na.rm = TRUE))
    y_arrow <- -0.10 * ymax

    dir_toward <- (arrow_df$direction_deg + 180) %% 360
    angle_rad <- (90 - dir_toward) * pi / 180

    arrow_df <- arrow_df |>
      mutate(
        y_arrow = y_arrow,
        xend = period_s + 0.05 * xspan * cos(angle_rad),
        yend = y_arrow + 0.07 * ymax * sin(angle_rad)
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
        format(
          spec_df$timestamp_utc[1],
          tz = "America/Los_Angeles",
          usetz = TRUE
        )
      ),
      x = "Period (s)",
      y = expression("Energy (" * m^2 / Hz * ")")
    ) +
    coord_cartesian(
      ylim = c(-0.16 * ymax, 1.12 * ymax),
      clip = "off"
    ) +
    theme_minimal() +
    theme(
      plot.margin = margin(10, 20, 30, 20)
    )

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
