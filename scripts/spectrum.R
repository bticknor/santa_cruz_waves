library(ggplot2)

spec <- read.table(
  "data/raw/46236.data_spec",
  header = TRUE,
  check.names = FALSE
)

latest <- spec[nrow(spec), ]
spec_cols <- setdiff(names(spec), c("YY", "MM", "DD", "hh", "mm", "Sep_Freq"))

freq_hz <- as.numeric(sub("^X", "", spec_cols))
energy <- as.numeric(latest[spec_cols])

df <- data.frame(
  frequency_hz = freq_hz,
  period_s = 1 / freq_hz,
  energy = energy
)

df <- df[is.finite(df$frequency_hz) & is.finite(df$period_s) & is.finite(df$energy), ]

p <- ggplot(df, aes(x = period_s, y = energy)) +
  geom_line() +
  scale_x_reverse() +
  labs(
    title = "46236 Swell Spectrum",
    x = "Period (s)",
    y = "Energy"
  ) +
  theme_minimal()

ggsave("46236_swell_spectrum.png", plot = p, width = 10, height = 6, dpi = 150)

