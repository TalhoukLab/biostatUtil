## This script obtains assignments into two clusters and consensus
## matrices from non-NMF-based algorithms that used consensus clustering via
## ConsensusClusterPlus (CCP)
## Author: Derek Chiu
## Dataset: pamr - khan

# Load packages and data
library(ConsensusClusterPlus)
library(ConClust)
library(pamr)
library(dplyr)
library(magrittr)
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
  scale %>%
  t

# Get consensus clusters
k <- 3 # only interested in k = 2
reps <- 1000
pItem <- 0.8

# HC Average Linkage Euclidean (takes ~ 1 sec)
system.time(hcAEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                distance = "euclidean",
                                seed = 123))

# HC Single Linkage Euclidean (takes ~ 1 sec)
hcSEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                innerLinkage = "single", distance = "euclidean",
                                seed = 123)

# HC Diana Euclidean (takes ~ 2 sec)
hcDianaEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                    clusterAlg = "dianaHook", distance = "euclidean",
                                    seed = 123)

# K-Means Euclidean (takes ~ 3 sec)
kmEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                               clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123)

# K-means Spearman (takes ~ 2 sec)
kmSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "kmdist", distance = "spearman",
                                seed = 123)

# K-means Mutual Information (takes ~ 2 sec)
kmMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                             clusterAlg = "kmdist", distance = "myMIdist",
                             seed = 123)

# PAM Euclidean (takes ~ 2 sec)
pamEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "pam", distance = "euclidean",
                                seed = 123)

# PAM Spearman (takes ~ 1 sec)
pamSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                 clusterAlg = "pam", distance = "spearman",
                                 seed = 123)

# PAM Mutual Information (takes ~ 2 sec)
pamMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                              clusterAlg = "pam", distance = "myMIdist",
                              seed = 123)

# Save ConsensusClusterPlus (CCP) results
saveRDS(list(hcAEucl, hcSEucl, hcDianaEucl, kmEucl, kmSpear,
             kmMI, pamEucl, pamSpear, pamMI),
        "Khan/outputs/results_CCP.rds", compress = "xz")
