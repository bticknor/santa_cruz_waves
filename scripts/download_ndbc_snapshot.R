# Read the CSV
buoys <- read.csv("/home/benjamin/Projects/santa_cruz_waves/data/buoys.csv", stringsAsFactors = FALSE)

# Extract station IDs
stations <- unique(buoys$id)

suffixes <- c("data_spec", "swdir", "swdir2", "swr1", "swr2")

dir.create("data/raw", recursive = TRUE, showWarnings = FALSE)

for (st in stations) {
  for (sx in suffixes) {
    url <- sprintf("https://www.ndbc.noaa.gov/data/realtime2/%s.%s", st, sx)
    dest <- sprintf("data/raw/%s.%s", st, sx)

    message("Downloading: ", url)
    tryCatch(
      {
        download.file(url, destfile = dest, mode = "wb", quiet = FALSE)
      },
      error = function(e) {
        message("Failed: ", url)
      }
    )
  }
}
