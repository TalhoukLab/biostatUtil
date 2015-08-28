## This script obtains assignments into two clusters and consensus
## matrices from non-NMF-based algorithms that used consensus clustering via
## ConsensusClusterPlus (CCP)
## Author: Derek Chiu
## Dataset: clusterSim::shapes.worms()

# Load packages
library(dplyr)
library(ConsensusClusterPlus)
library(ConClust)
library(clusterSim)
set.seed(1)
sw <- shapes.worms()

# Remove rows with low variability and scale
dat <- sw %>%
  use_series(data) %>%
  t %>%
  magrittr::extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale %>%
  t

# Get consensus clusters
k <- 3
reps <- 1000
pItem <- 0.8

# HC Average Linkage Euclidean (~ 27 secs)
hcAEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                distance = "euclidean",
                                seed = 123, verbose = T)

# HC Single Linkage Euclidean (~ 27 secs)
hcSEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                innerLinkage = "single", distance = "euclidean",
                                seed = 123, verbose = T)

# HC Diana Euclidean (~ 3.7 mins)
hcDianaEucl<- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                    clusterAlg = "dianaHook", distance = "euclidean",
                                    seed = 123, verbose = T)

# K-Means Euclidean (~ 1 min)
kmEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                               clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123, verbose = T)

# K-means Spearman (~ 1 min)
kmSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "kmdist", distance = "spearman",
                                seed = 123, verbose = T)

# K-means Mutual Information (~ 2 min)
kmMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                             clusterAlg = "kmdist", distance = "myMIdist",
                             seed = 123, verbose = T)

# PAM Euclidean (~ 1 min)
pamEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "pam", distance = "euclidean",
                                seed = 123, verbose = T)

# PAM Spearman (~ 44 secs)
pamSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                 clusterAlg = "pam", distance = "spearman",
                                 seed = 123, verbose = T)

# PAM Mutual Information (~ 1.5 mins)
pamMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                              clusterAlg = "pam", distance = "myMIdist",
                              seed = 123, verbose = T)

# Save ConsensusClusterPlus (CCP) results
saveRDS(list(hcAEucl, hcSEucl, hcDianaEucl, kmEucl, kmSpear,
             kmMI, pamEucl, pamSpear, pamMI),
        "Worms/outputs/results_CCP.rds", compress = "xz")
