library(leaflet)

# wind layer
add_wind_layer <- function(map, wind_df) {
  if (is.null(wind_df) || nrow(wind_df) == 0) return(map)

  pal <- colorNumeric("YlOrRd", domain = wind_df$speed_mps)

  for (i in seq_len(nrow(wind_df))) {
    map <- map %>%
      addPolylines(
        lng = c(wind_df$lng[i], wind_df$lng2[i]),
        lat = c(wind_df$lat[i], wind_df$lat2[i]),
        color = pal(wind_df$speed_mps[i]),
        weight = 2,
        opacity = 0.8,
        group = "Wind",
        label = sprintf(
          "Wind: %.1f m/s from %.0f°",
          wind_df$speed_mps[i],
          wind_df$direction_deg[i]
        )
      )
  }

  map
}


add_wind_arrows <- function(map, wind_df) {
  if (is.null(wind_df) || nrow(wind_df) == 0) return(map)

  pal <- colorNumeric("YlOrRd", domain = wind_df$speed_mps)

  # arrowhead size in degrees (small!)
  size <- 0.015

  for (i in seq_len(nrow(wind_df))) {

    # angle of arrow
    angle <- atan2(
      wind_df$lat2[i] - wind_df$lat[i],
      wind_df$lng2[i] - wind_df$lng[i]
    )

    # triangle points (arrowhead)
    tip_lng <- wind_df$lng2[i]
    tip_lat <- wind_df$lat2[i]

    left_lng <- tip_lng - size * cos(angle - pi / 6)
    left_lat <- tip_lat - size * sin(angle - pi / 6)

    right_lng <- tip_lng - size * cos(angle + pi / 6)
    right_lat <- tip_lat - size * sin(angle + pi / 6)

    map <- map %>%
      addPolygons(
        lng = c(tip_lng, left_lng, right_lng),
        lat = c(tip_lat, left_lat, right_lat),
        fillColor = pal(wind_df$speed_mps[i]),
        fillOpacity = 0.9,
        color = NA,
        group = "Wind"
      )
  }

  map
}
