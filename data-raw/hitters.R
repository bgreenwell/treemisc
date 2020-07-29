# Load and serialize
hitters <- read.csv("data-raw/hitters.csv", row.names = 1)
save(hitters, file = "data/hitters.rda")
