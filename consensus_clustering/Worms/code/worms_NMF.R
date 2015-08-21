## This script obtains assignments into two clusters and consensus
## matrices from NMF-based algorithms that used consensus clustering
## Author: Derek Chiu
## Dataset: clusterSim::shapes.worms()

# Load packages and data
library(plyr)
library(ConClust)
library(clusterSim)
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

# Cluster memberships using HC ... how about using ward.D?
classes <- llply(con.mats, function(x) {
  cl <- as.factor(cutree(hclust(dist(x), method = "ward.D"), k))
  names(cl) <- hclust(dist(x), method = "ward.D")$labels
  return(cl)})

# Save NMF results
saveRDS(list(nmfDiv = list(consensusMatrix = con.mats$nmfDiv,
                           consensusClass = classes$nmfDiv),
             nmfEucl = list(consensusMatrix = con.mats$nmfEucl,
                            consensusClass = classes$nmfEucl)),
        "Worms/outputs/results_NMF.rds", compress = "xz")
