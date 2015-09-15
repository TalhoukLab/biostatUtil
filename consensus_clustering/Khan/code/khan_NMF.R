## This script obtains assignments into four clusters and consensus
## matrices from NMF-based algorithms that used consensus clustering
## Author: Derek Chiu
## Dataset: UCI - mice

# Load packages and data
library(plyr)
library(ConClust)
library(pamr)
k <- 2
set.seed(1)
n.samp <- 16
data(khan)
dat <- khan %>%
  set_rownames(.$X) %>%
  select(-X, -X1) %>%
  set_colnames(paste0(names(.), "_", as.character(unlist(.[1, ])))) %>%
  extract(-1, ) %>%
  apply(., 2, function(x) as.numeric(as.character(x))) %>%
  as.data.frame %>%
  extract(, c(sample(grep("EWS", names(.)), n.samp),
              sample(grep("RMS", names(.)), n.samp))) %>%
  extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale

# Get consensus clusters (takes ~ 18 mins)
ConClust(x = dat, k = k, pItem = 0.8, reps = 1000,
         method = c("nmfDiv", "nmfEucl"), dir = "Khan/outputs/")
results <- readRDS("Khan/outputs/ConClustOutput_2015-09-15.rds")

# Consensus matrices (takes ~ 23 secs)
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
        "Khan/outputs/results_NMF.rds", compress = "xz")
