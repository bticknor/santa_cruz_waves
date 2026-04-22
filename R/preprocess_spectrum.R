library(pracma)
library(dplyr)

preprocess_spectrum <- function(spec_file, swdir_file, period_min = 3, period_max = 25) {
  cache_file <- file.path("data/processed", paste0(basename(spec_file), ".rds"))

  if (file.exists(cache_file)) {
    raw_mtime <- max(file.mtime(spec_file), file.mtime(swdir_file))
    if (file.mtime(cache_file) > raw_mtime) {
      return(readRDS(cache_file))
    }
  }

  spec_df <- read_ndbc_spec(spec_file, period_min, period_max)
  dir_df  <- read_ndbc_swdir(swdir_file, period_min, period_max)

  spec_dat <- spec_df |>
    left_join(
      dir_df |> select(frequency_hz, direction_deg),
      by = "frequency_hz"
    ) |>
    arrange(period_s)

  if (nrow(spec_dat) == 0) stop("No usable spectrum data after filtering")

  spec_dat$freq_step <- c(diff(spec_dat$frequency_hz), NA_real_)
  fallback_step <- median(abs(diff(spec_dat$frequency_hz)), na.rm = TRUE)
  if (!is.finite(fallback_step)) fallback_step <- 0
  spec_dat$freq_step[is.na(spec_dat$freq_step)] <- fallback_step
  spec_dat$freq_step <- abs(spec_dat$freq_step)

  smooth_k <- 2
  smooth_vals <- stats::filter(spec_dat$energy, rep(1 / smooth_k, smooth_k), sides = 2)
  spec_dat$energy_smooth <- as.numeric(smooth_vals)
  spec_dat$energy_smooth[is.na(spec_dat$energy_smooth)] <- spec_dat$energy[is.na(spec_dat$energy_smooth)]

  peaks <- findpeaks(
    spec_dat$energy_smooth,
    nups = 1,
    ndowns = 1,
    minpeakheight = max(spec_dat$energy_smooth, na.rm = TRUE) * 0.12
  )

  if (is.null(peaks)) {
    peak_df <- data.frame(
      period_s      = numeric(),
      energy        = numeric(),
      energy_smooth = numeric(),
      direction_deg = numeric(),
      freq_step     = numeric(),
      m0            = numeric(),
      Hs_peak       = numeric(),
      power_raw     = numeric(),
      power_pct     = numeric(),
      dir_label     = character(),
      label         = character()
    )
  } else {
    peak_idx <- peaks[, 2]
    peak_df <- spec_dat[peak_idx, c("period_s", "energy", "energy_smooth", "direction_deg")]
    peak_df <- peak_df[order(-peak_df$energy_smooth), ]

    peak_df <- peak_df |>
      left_join(spec_dat |> select(period_s, freq_step), by = "period_s") |>
      mutate(
        m0_raw    = energy_smooth * freq_step,
        m0        = ifelse(is.finite(m0_raw), pmax(m0_raw, 0), 0),
        Hs_peak   = 4 * sqrt(m0) * 3.28084,
        power_raw = ifelse(is.finite(energy_smooth), pmax(energy_smooth, 0) * period_s, 0)
      ) |>
      select(-m0_raw)

    total_power <- sum(peak_df$power_raw, na.rm = TRUE)

    peak_df <- peak_df |>
      mutate(
        power_pct = if (total_power > 0) 100 * power_raw / total_power else NA_real_,
        dir_label = ifelse(is.na(direction_deg), "", deg_to_compass(direction_deg)),
        label     = ifelse(
          dir_label == "",
          sprintf("%.0f%%\n%.1fs\n%.1fft", power_pct, period_s, Hs_peak),
          sprintf("%.0f%%\n%.1fs %s\n%.1fft", power_pct, period_s, dir_label, Hs_peak)
        )
      )
  }

  result <- list(
    spec_dat      = spec_dat,
    peak_df       = peak_df,
    timestamp_utc = spec_df$timestamp_utc[1]
  )

  dir.create(dirname(cache_file), showWarnings = FALSE, recursive = TRUE)
  saveRDS(result, cache_file)

  result
}

deg_to_compass <- function(deg) {
  dirs <- c("N", "NE", "E", "SE", "S", "SW", "W", "NW")
  ix <- round((deg %% 360) / 45) %% 8 + 1
  dirs[ix]
}

read_ndbc_spec <- function(file, period_min, period_max) {
  lines      <- readLines(file, warn = FALSE)
  data_lines <- lines[!grepl("^#", lines)]

  if (length(data_lines) == 0) stop("No data rows in spec file")

  parts    <- scan(text = data_lines[1], what = character(), quiet = TRUE)
  sep_freq <- as.numeric(parts[6])

  timestamp_utc <- as.POSIXct(
    sprintf("%04d-%02d-%02d %02d:%02d",
            as.integer(parts[1]), as.integer(parts[2]),
            as.integer(parts[3]), as.integer(parts[4]),
            as.integer(parts[5])),
    format = "%Y-%m-%d %H:%M",
    tz     = "UTC"
  )

  spec_parts  <- parts[-(1:6)]
  energy_vals <- spec_parts[seq(1, length(spec_parts), by = 2)]
  freq_labels <- spec_parts[seq(2, length(spec_parts), by = 2)]

  out <- data.frame(
    timestamp_utc = timestamp_utc,
    frequency_hz  = as.numeric(gsub("[()]", "", freq_labels)),
    energy        = as.numeric(energy_vals),
    sep_freq      = sep_freq
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

read_ndbc_swdir <- function(file, period_min, period_max) {
  lines      <- readLines(file, warn = FALSE)
  data_lines <- lines[!grepl("^#", lines)]

  if (length(data_lines) == 0) stop("No data rows in swdir file")

  parts <- scan(text = data_lines[1], what = character(), quiet = TRUE)

  timestamp_utc <- as.POSIXct(
    sprintf("%04d-%02d-%02d %02d:%02d",
            as.integer(parts[1]), as.integer(parts[2]),
            as.integer(parts[3]), as.integer(parts[4]),
            as.integer(parts[5])),
    format = "%Y-%m-%d %H:%M",
    tz     = "UTC"
  )

  dir_parts   <- parts[-(1:5)]
  dir_vals    <- dir_parts[seq(1, length(dir_parts), by = 2)]
  freq_labels <- dir_parts[seq(2, length(dir_parts), by = 2)]

  out <- data.frame(
    timestamp_utc = timestamp_utc,
    frequency_hz  = as.numeric(gsub("[()]", "", freq_labels)),
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
      direction_deg = ifelse(direction_deg < 0 | direction_deg > 360, NA_real_, direction_deg)
    ) |>
    arrange(period_s)
}
