## This script obtains assignments into four clusters and consensus
## matrices from NMF-based algorithms that used consensus clustering
## Author: Derek Chiu

# Load packages and data
library(plyr)
library(ConClust)
set.seed(1)
sw <- shapes.worms()
k <- 2

# Get consensus clusters (~ 54 mins)
# ConClust(x = t(sw$data), k = k, pItem = 0.8, reps = 1000,
#          method = c("nmfDiv", "nmfEucl"), dir = "Worms/outputs/")
results <- readRDS("Worms/outputs/ConClustOutput_2015-08-18.rds")

# Consensus matrices (takes ~ 3 mins)
con.mats <- alply(results, 3, consensusMatrix,
                  .progress = "text", .dims = TRUE)

# Cluster memberships using HC
classes <- llply(con.mats, function(x) {
  cl <- as.factor(cutree(hclust(dist(x), method = "average"), k))
  names(cl) <- hclust(dist(x), method = "average")$labels
  return(cl)})

# Save NMF results
saveRDS(list(nmfDiv = list(consensusMatrix = con.mats$nmfDiv,
                           consensusClass = classes$nmfDiv),
             nmfEucl = list(consensusMatrix = con.mats$nmfEucl,
                            consensusClass= classes$nmfEucl)),
        "Worms/outputs/results_NMF.rds", compress = "xz")
