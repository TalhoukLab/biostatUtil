## This script obtains assignments into three clusters and consensus
## matrices from non-NMF-based algorithms that used consensus clustering via
## ConsensusClusterPlus (CCP)
## Author: Derek Chiu
## Dataset: UCI - seeds

# Load packages and data
library(ConsensusClusterPlus)
library(ConClust)
seeds <- readRDS("Seeds/seeds.rds")

# Remove features with low variability and scale
dat <- seeds %>%
  dplyr::select(which(sapply(., class) == "numeric")) %>%
  t %>%
  magrittr::extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale %>%
  t

# Get consensus clusters
k <- 3
reps <- 1000
pItem <- 0.8

# HC Average Linkage Euclidean (takes ~ 10 sec)
hcAEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                distance = "euclidean",
                                seed = 123, verbose = T)

# HC Single Linkage Euclidean (takes ~ 10 sec)
hcSEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                innerLinkage = "single", distance = "euclidean",
                                seed = 123, verbose = T)

# HC Diana Euclidean (takes ~ 30 sec)
hcDianaEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                    clusterAlg = "dianaHook", distance = "euclidean",
                                    seed = 123, verbose = T)

# K-Means Euclidean (takes ~ 19 sec)
kmEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                               clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123, verbose = T)

# K-means Spearman (takes ~ 30 sec)
kmSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "kmdist", distance = "spearman",
                                seed = 123, verbose = T)

# K-means Mutual Information (takes ~ 45 sec)
kmMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                             clusterAlg = "kmdist", distance = "myMIdist",
                             seed = 123, verbose = T)

# PAM Euclidean (takes ~ 17 sec)
pamEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "pam", distance = "euclidean",
                                seed = 123, verbose = T)

# PAM Spearman (takes ~ 15 sec)
pamSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                 clusterAlg = "pam", distance = "spearman",
                                 seed = 123, verbose = T)

# PAM Mutual Information (takes ~ 29 sec)
pamMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                              clusterAlg = "pam", distance = "myMIdist",
                              seed = 123, verbose = T)

# Save ConsensusClusterPlus (CCP) results
saveRDS(list(hcAEucl, hcSEucl, hcDianaEucl, kmEucl, kmSpear,
             kmMI, pamEucl, pamSpear, pamMI),
        "Seeds/outputs/results_CCP.rds", compress = "xz")
