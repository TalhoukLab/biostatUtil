## This script obtains assignments into four clusters and consensus
## matrices from NMF-based algorithms that used consensus clustering
## Author: Derek Chiu

# Load packages and data
library(plyr)
library(ConClust)
k <- 4
dat.raw <- read.csv("Mice/Data_Cortex_Nuclear.csv")
dat <- dat.raw %>%
  filter(is_in(class, c("c-CS-m", "c-CS-s", "c-SC-m", "c-SC-s"))) %>%
  select(which(sapply(., class) == "numeric")) %>%
  t %>%
  as.data.frame %>%
  set_colnames(dat.raw$MouseID[dat.raw$class %in%
                                 c("c-CS-m", "c-CS-s", "c-SC-m", "c-SC-s")])

# Get consensus clusters (takes ~ 2 hours)
# No restriction of feature space
ConClust(x = dat, k = k, pItem = 0.8, reps = 1000, min.sd = 0,
         method = c("nmfDiv", "nmfEucl"), dir = "Mice/outputs/")
results <- readRDS("Mice/outputs/ConClustOutput_2015-09-11.rds")

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
        "Mice/outputs/results_NMF.rds", compress = "xz")
