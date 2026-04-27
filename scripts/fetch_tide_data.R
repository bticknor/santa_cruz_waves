# Fetch tide predictions for Santa Cruz (station 9413745) from NOAA CO-OPS.
#
# Station 9413745 is a subordinate station with no independent datums. Predictions
# are derived from the Monterey reference station (9413450) with NOAA-published
# Santa Cruz offsets applied:
#   High tide: time -6 min, height x0.97
#   Low tide:  time -11 min, height x0.99
#
# Saves to data/raw/tide_latest.rds.
#
# Run from project root:
#   Rscript scripts/fetch_tide_data.R

library(jsonlite)

REF_STATION  <- "9413450"   # Monterey — reference harmonic station for Santa Cruz
HI_TIME_OFF  <- -6          # minutes
LO_TIME_OFF  <- -11         # minutes
HI_HT_SCALE  <- 0.97
LO_HT_SCALE  <- 0.99

fetch_predictions <- function(begin_utc, end_utc, interval = "") {
  url <- paste0(
    "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter",
    "?station=",    REF_STATION,
    "&product=predictions",
    "&datum=MLLW",
    "&time_zone=gmt",
    "&units=english",
    "&format=json",
    if (nchar(interval)) paste0("&interval=", interval) else "",
    "&begin_date=", format(begin_utc, "%Y%m%d", tz = "UTC"),
    "&end_date=",   format(end_utc,   "%Y%m%d", tz = "UTC")
  )
  tryCatch(
    fromJSON(url),
    error = function(e) { message("  API error: ", conditionMessage(e)); NULL }
  )
}

parse_utc <- function(t_str) {
  as.POSIXct(t_str, format = "%Y-%m-%d %H:%M", tz = "UTC")
}

now_utc   <- as.POSIXct(Sys.time(), tz = "UTC")
begin_utc <- now_utc - 3 * 3600    # 3 hours back
end_utc   <- now_utc + 8 * 3600    # 8 hours ahead

# Hourly curve from Monterey — apply average offset for Santa Cruz
curve_raw <- fetch_predictions(begin_utc, end_utc, interval = "h")

curve <- NULL
if (!is.null(curve_raw) && !is.null(curve_raw$predictions)) {
  avg_time_off <- mean(c(HI_TIME_OFF, LO_TIME_OFF))   # -8.5 min
  avg_ht_scale <- mean(c(HI_HT_SCALE, LO_HT_SCALE))  # 0.98
  curve <- data.frame(
    time           = parse_utc(curve_raw$predictions$t) + avg_time_off * 60,
    water_level_ft = suppressWarnings(as.numeric(curve_raw$predictions$v)) * avg_ht_scale,
    stringsAsFactors = FALSE
  )
  curve <- curve[!is.na(curve$water_level_ft), ]
  message("  Curve: ", nrow(curve), " points")
} else {
  message("  No curve data returned")
}

# Hi/lo predictions — apply per-type Santa Cruz offsets for accurate labels
# Expand hilo window slightly so tides just outside the curve window are captured
hilo_raw <- fetch_predictions(begin_utc - 6 * 3600, end_utc + 6 * 3600, interval = "hilo")

hilo <- NULL
if (!is.null(hilo_raw) && !is.null(hilo_raw$predictions)) {
  df <- data.frame(
    time           = parse_utc(hilo_raw$predictions$t),
    water_level_ft = suppressWarnings(as.numeric(hilo_raw$predictions$v)),
    type           = hilo_raw$predictions$type,
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$water_level_ft), ]

  df$time <- ifelse(
    df$type == "H",
    df$time + HI_TIME_OFF * 60,
    df$time + LO_TIME_OFF * 60
  )
  df$time <- as.POSIXct(df$time, origin = "1970-01-01", tz = "UTC")

  df$water_level_ft <- ifelse(
    df$type == "H",
    df$water_level_ft * HI_HT_SCALE,
    df$water_level_ft * LO_HT_SCALE
  )

  hilo <- df
  message("  Hi/Lo: ", nrow(hilo), " points")
} else {
  message("  No hi/lo data returned")
}

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)
saveRDS(
  list(curve = curve, hilo = hilo, fetched_at = now_utc),
  "data/raw/tide_latest.rds"
)
message("  Tide data saved to data/raw/tide_latest.rds")
