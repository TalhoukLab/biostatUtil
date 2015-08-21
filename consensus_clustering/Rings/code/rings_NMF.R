## This script obtains assignments into four clusters and consensus
## matrices from NMF-based algorithms that used consensus clustering
## Author: Derek Chiu
## Dataset: clusterSim::shapes.circles3()

# Load packages and data
library(plyr)
library(ConClust)
library(clusterSim)
set.seed(1)
sc3 <- shapes.circles3()
k <- 3

# Get consensus clusters (~ takes 4 hours)
# ConClust(x = t(sc3$data), k = k, pItem = 0.8, reps = 1000, min.sd = 0,
#          method = c("nmfDiv", "nmfEucl"), dir = "Rings/outputs/")
results <- readRDS("Rings/outputs/ConClustOutput_2015-08-20.rds")

# Consensus matrices (takes ~ 6 mins)
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
        "Rings/outputs/results_NMF.rds", compress = "xz")
