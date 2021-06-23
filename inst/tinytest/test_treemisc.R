# Load required packages
library(Matrix)
library(ranger)

# Load Ames housing data
ames <- as.data.frame(AmesHousing::make_ames())

# Fit a ranfom forest to the Ames housing data
set.seed(822)  # for reproducibility
rfo <- ranger(Sale_Price ~ ., data = ames)

# Compute proximity matrix
system.time(
  ames_prox <- proximity(rfo, data = ames) 
)

# Check size
format(object.size(ames_prox), units = "MB")

# Visualize proximities
image(ames_prox)


mds <- stats::cmdscale(1 - ames_prox, eig = TRUE, k = 2)
plot(mds$points)
