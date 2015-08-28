## This script obtains assignments into two clusters and consensus
## matrices from non-NMF-based algorithms that used consensus clustering via
## ConsensusClusterPlus (CCP)
## Author: Derek Chiu
## Dataset: clusterSim::shapes.circles3()

# Load packages
library(dplyr)
library(ConsensusClusterPlus)
library(ConClust)
library(clusterSim)
set.seed(1)
sc3 <- shapes.circles3()

# Just scale.
dat <- sc3 %>%
  use_series(data) %>%
  scale %>%
  t

# Get consensus clusters
k <- 3
reps <- 1000
pItem <- 0.8

# HC Average Linkage Euclidean (~ 90 secs)
hcAEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                distance = "euclidean",
                                seed = 123, verbose = T)

# HC Single Linkage Euclidean (~ 90 secs)
hcSEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                innerLinkage = "single", distance = "euclidean",
                                seed = 123, verbose = T)

# HC Diana Euclidean (~ 7 mins)
hcDianaEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                    clusterAlg = "dianaHook", distance = "euclidean",
                                    seed = 123, verbose = T)

# K-Means Euclidean (~ 1.5 min)
kmEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                               clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123, verbose = T)

# K-means Spearman (~ 2 min)
kmSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "kmdist", distance = "spearman",
                                seed = 123, verbose = T)

# K-means Mutual Information (~ 4 min)
kmMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                             clusterAlg = "kmdist", distance = "myMIdist",
                             seed = 123, verbose = T)

# PAM Euclidean (~ 2 min)
pamEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "pam", distance = "euclidean",
                                seed = 123, verbose = T)

# PAM Spearman (~ 1.5 secs)
pamSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                 clusterAlg = "pam", distance = "spearman",
                                 seed = 123, verbose = T)

# PAM Mutual Information (doesn't work)
# pamMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
#                               clusterAlg = "pam", distance = "myMIdist",
#                               seed = 123, verbose = T)

# Save ConsensusClusterPlus (CCP) results
saveRDS(list(hcAEucl, hcSEucl, hcDianaEucl, kmEucl, kmSpear,
             kmMI, pamEucl, pamSpear),
        "Rings/outputs/results_CCP.rds", compress = "xz")
