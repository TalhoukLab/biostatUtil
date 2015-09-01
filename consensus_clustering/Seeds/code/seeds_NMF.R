## This script obtains assignments into three clusters and consensus
## matrices from NMF-based algorithms that used consensus clustering
## Author: Derek Chiu
## Dataset: UCI - seeds

# Load packages and data
library(plyr)
library(ConClust)
seeds <- readRDS("Seeds/seeds.rds")
k <- 3

# Get consensus clusters (~ 31 mins)
# ConClust(x = t(seeds), k = k, pItem = 0.8, reps = 1000,
#          method = c("nmfDiv", "nmfEucl"), dir = "Seeds/outputs/")
results <- readRDS("Seeds/outputs/ConClustOutput_2015-09-01.rds")

# Consensus matrices (takes ~ 1.2 min)
con.mats <- alply(results, 3, consensusMatrix,
                  .progress = "text", .dims = TRUE)

# Cluster memberships using HC
classes <- llply(con.mats, function(x) {
  cl <- as.factor(cutree(hclust(dist(x), method = "ward.D"), k))
  names(cl) <- hclust(dist(x), method = "ward.D")$labels
  return(cl)})

# Save NMF results
saveRDS(list(nmfDiv = list(consensusMatrix = con.mats$nmfDiv,
                           consensusClass = classes$nmfDiv),
             nmfEucl = list(consensusMatrix = con.mats$nmfEucl,
                            consensusClass = classes$nmfEucl)),
        "Seeds/outputs/results_NMF.rds", compress = "xz")
