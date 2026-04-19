library(jsonlite)
library(dplyr)
library(purrr)

fetch_openmeteo_wind_point <- function(lat, lon) {
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

  if (is.null(x$current) ||
      is.null(x$current$wind_speed_10m) ||
      is.null(x$current$wind_direction_10m)) {
    return(data.frame(
      lat = numeric(0),
      lng = numeric(0),
      speed_mps = numeric(0),
      direction_deg = numeric(0)
    ))
  }

  data.frame(
    lat = lat,
    lng = lon,
    speed_mps = x$current$wind_speed_10m,
    direction_deg = x$current$wind_direction_10m
  )
}

build_static_wind_field <- function(
  lats = seq(35.9, 37.7, by = 0.2),
  lngs = seq(-123.4, -121.1, by = 0.2),
  arrow_km = 15
) {
  pts <- expand.grid(lat = lats, lng = lngs)

  wind <- purrr::map2_dfr(pts$lat, pts$lng, fetch_openmeteo_wind_point)

  if (nrow(wind) == 0) {
    stop("No wind data returned from Open-Meteo")
  }

  # meteorological direction is where wind comes from
  # arrows should point toward where the wind is going
  dir_toward <- (wind$direction_deg + 180) %% 360
  angle_rad <- (90 - dir_toward) * pi / 180

  max_speed <- max(wind$speed_mps, na.rm = TRUE)
  if (!is.finite(max_speed) || max_speed <= 0) max_speed <- 1

  wind <- wind |>
    mutate(
      dlat = (speed_mps / max_speed) * (arrow_km / 111) * sin(angle_rad),
      dlng = (speed_mps / max_speed) * (arrow_km / (111 * cos(lat * pi / 180))) * cos(angle_rad),
      lat2 = lat + dlat,
      lng2 = lng + dlng
    )

  wind
}

wind <- build_static_wind_field(
  lats = seq(36.0, 37.6, by = 0.3),
  lngs = seq(-123.3, -121.2, by = 0.3),
  arrow_km = 12
)

saveRDS(wind, "/home/benjamin/Projects/santa_cruz_waves/data/raw/wind_latest.rds")
