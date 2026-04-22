library(leaflet)

MAX_WIND_MPH <- 20
MAX_WIND_MPS <- MAX_WIND_MPH * 0.44704

wind_pal <- colorNumeric("YlOrRd", domain = c(0, MAX_WIND_MPS))

add_wind_layer <- function(map, wind_df) {
  if (is.null(wind_df) || nrow(wind_df) == 0) return(map)

  for (i in seq_len(nrow(wind_df))) {
    map <- map %>%
      addPolylines(
        lng     = c(wind_df$lng[i], wind_df$lng2[i]),
        lat     = c(wind_df$lat[i], wind_df$lat2[i]),
        color   = wind_pal(wind_df$speed_mps[i]),
        weight  = 2,
        opacity = 0.8,
        group   = "Wind",
        label   = sprintf(
          "Wind: %.0f mph (%.1f m/s) from %.0f°",
          wind_df$speed_mps[i] / 0.44704,
          wind_df$speed_mps[i],
          wind_df$direction_deg[i]
        )
      )
  }

  map
}


add_wind_arrows <- function(map, wind_df) {
  if (is.null(wind_df) || nrow(wind_df) == 0) return(map)

  size <- 0.015

  for (i in seq_len(nrow(wind_df))) {
    angle <- atan2(
      wind_df$lat2[i] - wind_df$lat[i],
      wind_df$lng2[i] - wind_df$lng[i]
    )

    tip_lng <- wind_df$lng2[i]
    tip_lat <- wind_df$lat2[i]

    map <- map %>%
      addPolygons(
        lng         = c(tip_lng,
                        tip_lng - size * cos(angle - pi / 6),
                        tip_lng - size * cos(angle + pi / 6)),
        lat         = c(tip_lat,
                        tip_lat - size * sin(angle - pi / 6),
                        tip_lat - size * sin(angle + pi / 6)),
        fillColor   = wind_pal(wind_df$speed_mps[i]),
        fillOpacity = 0.9,
        color       = NA,
        group       = "Wind"
      )
  }

  map
}


add_wind_legend <- function(map) {
  speeds_mph <- c(5, 10, 15, 20)
  max_px     <- 80

  rows <- sapply(speeds_mph, function(mph) {
    px    <- round(mph / MAX_WIND_MPH * max_px)
    color <- wind_pal(mph * 0.44704)
    sprintf(
      '<div style="display:flex;align-items:center;margin:3px 0">
         <span style="width:36px;text-align:right;padding-right:8px;font-size:11px">%d mph</span>
         <div style="width:%dpx;height:3px;background:%s;border-radius:1px"></div>
       </div>',
      mph, px, color
    )
  })

  html <- sprintf(
    '<div id="wind-legend" style="background:white;padding:8px 10px;border-radius:4px;
                 box-shadow:0 1px 4px rgba(0,0,0,0.3);font-family:sans-serif">
       <div style="font-weight:bold;font-size:12px;margin-bottom:5px">Wind Speed</div>
       %s
     </div>',
    paste(rows, collapse = "\n")
  )

  map %>%
    addControl(html = html, position = "bottomright") %>%
    htmlwidgets::onRender("
      function(el, x) {
        var map = this;
        map.on('overlayadd', function(e) {
          if (e.name === 'Wind') {
            var legend = document.getElementById('wind-legend');
            if (legend) legend.style.display = 'block';
          }
        });
        map.on('overlayremove', function(e) {
          if (e.name === 'Wind') {
            var legend = document.getElementById('wind-legend');
            if (legend) legend.style.display = 'none';
          }
        });
      }
    ")
}
