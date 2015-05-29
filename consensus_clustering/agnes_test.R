source("consensus_clustering/functions/distance_functions.R")

set.seed(123)
test <- matrix(rnorm(1000), ncol = 10)
test <- matrix(runif(1000), ncol = 10)

ass1a <- cutree(agnes(dist(test), diss = TRUE), 4)
ass1b <- cutree(agnes(test, diss = FALSE), 4)
ass2 <- cutree(hclust(dist(test), method = "average"), 4)

identical(ass1a, ass2)
identical(ass1b, ass2)