## This script obtains assignments into three clusters and consensus
## matrices from non-NMF-based algorithms that used consensus clustering via
## ConsensusClusterPlus (CCP)
## Author: Derek Chiu
## Dataset: UCI - mice

# Load packages and data
library(ConsensusClusterPlus)
library(ConClust)
dat.raw <- read.csv("Mice/Data_Cortex_Nuclear.csv")
dat <- dat.raw %>%
  filter(is_in(class, c("c-CS-m", "c-CS-s", "c-SC-m", "c-SC-s"))) %>%
  select(which(sapply(., class) == "numeric")) %>%
  t %>%
  as.data.frame %>%
  set_colnames(dat.raw$MouseID[dat.raw$class %in%
                                 c("c-CS-m", "c-CS-s", "c-SC-m", "c-SC-s")]) %>%
  extract(, apply(., 2, function(x) !any(is.na(x)))) %>%
  t %>%
  scale %>%
  t

# Get consensus clusters
k <- 4
reps <- 1000
pItem <- 0.8

# HC Average Linkage Euclidean (takes ~ 25 sec)
hcAEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                distance = "euclidean",
                                seed = 123, verbose = T)

# HC Single Linkage Euclidean (takes ~ 25 sec)
hcSEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                innerLinkage = "single", distance = "euclidean",
                                seed = 123, verbose = T)

# HC Diana Euclidean (takes ~ 75 sec)
hcDianaEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                    clusterAlg = "dianaHook", distance = "euclidean",
                                    seed = 123, verbose = T)

# K-Means Euclidean (takes ~ 54 sec)
kmEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                               clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123, verbose = T)

# K-means Spearman (takes ~ 51 sec)
kmSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "kmdist", distance = "spearman",
                                seed = 123, verbose = T)

# K-means Mutual Information (takes ~ 77 sec)
kmMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                             clusterAlg = "kmdist", distance = "myMIdist",
                             seed = 123, verbose = T)

# PAM Euclidean (takes ~ 42 sec)
pamEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "pam", distance = "euclidean",
                                seed = 123, verbose = T)

# PAM Spearman (takes ~ 45 sec)
pamSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                 clusterAlg = "pam", distance = "spearman",
                                 seed = 123, verbose = T)

# PAM Mutual Information (takes ~ 63 sec)
pamMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                              clusterAlg = "pam", distance = "myMIdist",
                              seed = 123, verbose = T)

# Save ConsensusClusterPlus (CCP) results
saveRDS(list(hcAEucl, hcSEucl, hcDianaEucl, kmEucl, kmSpear,
             kmMI, pamEucl, pamSpear, pamMI),
        "Mice/outputs/results_CCP.rds", compress = "xz")
