# Full data update pipeline: download raw data, preprocess spectra, download wind.
#
# Run from project root:
#   Rscript scripts/update_data.R
#
# Cron example (every 2 hours):
#   0 */2 * * * cd /home/benjamin/Projects/santa_cruz_waves && Rscript scripts/update_data.R >> logs/update_data.log 2>&1

library(jsonlite)
library(dplyr)
library(purrr)

source("scripts/preprocess_spectrum.R")

buoys <- read.csv("data/buoys.csv", stringsAsFactors = FALSE)
stations <- unique(buoys$id)

dir.create("data/raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data/processed", recursive = TRUE, showWarnings = FALSE)
dir.create("logs",           recursive = TRUE, showWarnings = FALSE)

# --- Download NDBC buoy data ---

for (st in stations) {
  for (sx in c("data_spec", "swdir")) {
    url  <- sprintf("https://www.ndbc.noaa.gov/data/realtime2/%s.%s", st, sx)
    dest <- sprintf("data/raw/%s.%s", st, sx)
    message(format(Sys.time()), " Downloading: ", url)
    tryCatch(
      download.file(url, destfile = dest, mode = "wb", quiet = TRUE),
      error = function(e) message(format(Sys.time()), " Failed:      ", url, " — ", conditionMessage(e))
    )
  }
}

# --- Preprocess spectra ---

for (st in stations) {
  spec_file  <- sprintf("data/raw/%s.data_spec", st)
  swdir_file <- sprintf("data/raw/%s.swdir", st)

  if (!file.exists(spec_file) || !file.exists(swdir_file)) {
    message(format(Sys.time()), " Skipping preprocessing for ", st, ": missing raw files")
    next
  }

  message(format(Sys.time()), " Preprocessing: ", st)
  tryCatch(
    preprocess_spectrum(spec_file, swdir_file),
    error = function(e) message(format(Sys.time()), " Failed preprocessing ", st, ": ", conditionMessage(e))
  )
}

# --- Download wind data ---

fetch_wind_point <- function(lat, lon) {
  url <- sprintf(
    paste0(
      "https://api.open-meteo.com/v1/forecast?",
      "latitude=%f&longitude=%f",
      "&current=wind_speed_10m,wind_direction_10m",
      "&wind_speed_unit=ms"
    ),
    lat, lon
  )
  x <- jsonlite::fromJSON(url)
  if (is.null(x$current$wind_speed_10m) || is.null(x$current$wind_direction_10m)) {
    return(data.frame(lat = numeric(0), lng = numeric(0),
                      speed_mps = numeric(0), direction_deg = numeric(0)))
  }
  data.frame(lat = lat, lng = lon,
             speed_mps = x$current$wind_speed_10m,
             direction_deg = x$current$wind_direction_10m)
}

message(format(Sys.time()), " Downloading wind data")
tryCatch({
  pts  <- expand.grid(lat = seq(36.0, 37.6, by = 0.3),
                      lng = seq(-123.3, -121.2, by = 0.3))
  wind <- purrr::map2_dfr(pts$lat, pts$lng, fetch_wind_point)

  if (nrow(wind) == 0) stop("No wind data returned from Open-Meteo")

  arrow_km      <- 12
  max_speed_ref <- 20 * 0.44704  # 20 mph in m/s — full-length arrow at this speed
  dir_toward    <- (wind$direction_deg + 180) %% 360
  angle_rad     <- (90 - dir_toward) * pi / 180

  wind <- wind |>
    mutate(
      dlat = (speed_mps / max_speed_ref) * (arrow_km / 111) * sin(angle_rad),
      dlng = (speed_mps / max_speed_ref) * (arrow_km / (111 * cos(lat * pi / 180))) * cos(angle_rad),
      lat2 = lat + dlat,
      lng2 = lng + dlng
    )

  saveRDS(wind, "data/raw/wind_latest.rds")
  message(format(Sys.time()), " Wind data saved")
},
error = function(e) message(format(Sys.time()), " Failed wind download: ", conditionMessage(e))
)

# --- Download tide data ---

message(format(Sys.time()), " Downloading tide data")
tryCatch(
  source("scripts/fetch_tide_data.R"),
  error = function(e) message(format(Sys.time()), " Failed tide download: ", conditionMessage(e))
)

message(format(Sys.time()), " Update complete")
