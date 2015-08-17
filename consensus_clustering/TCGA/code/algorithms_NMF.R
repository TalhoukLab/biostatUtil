## This script obtains assignments into four clusters and consensus
## matrices from NMF-based algorithms that used consensus clustering
## Author: Derek Chiu

# Load packages and data
library(plyr)
library(ConClust)
library(tcgaHGSC)
data(hgsc)
k <- 4

# Get consensus clusters (takes very long to run!)
# ConClust(x = hgsc, k = k, pItem = 0.8, reps = 1000,
#          method = c("nmfDiv", "nmfEucl"), dir = "TCGA/outputs/")
results <- readRDS("TCGA/outputs/nmf_output_05-19-2015.rds")

# Consensus matrices (takes ~ 5 mins)
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
        "TCGA/outputs/results_NMF.rds", compress = "xz")
