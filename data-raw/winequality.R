# Combine data sets and serialize
red <- read.csv("data-raw/winequality-red.csv", sep = ";")
white <- read.csv("data-raw/winequality-white.csv", sep = ";")
wine <- rbind(
  cbind(red, "type" = "red"),
  cbind(white, "type" = "white")
)
save(wine, file = "data/wine.rda")
