## This script obtains assignments into four clusters and consensus
## matrices from non-NMF-based algorithms that used consensus clustering via
## ConsensusClusterPlus (CCP)
## Author: Derek Chiu

# Load packages and data
library(dplyr)
library(magrittr)
library(ConsensusClusterPlus)
library(ConClust)
library(tcgaHGSC)
data(hgsc)

# Remove genes with low variability and scale on the gene space
dat <- hgsc %>%
  set_rownames(.$UNIQID) %>%
  select(which(sapply(., class) == "numeric")) %>%
  extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale %>%
  t

# Get consensus clusters
k <- 4
reps <- 1000
pItem <- 0.8

# HC Average Linkage Euclidean (takes ~ 1.2 mins)
hcAEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                distance = "euclidean",
                                seed = 123, verbose = T)

# HC Single Linkage Euclidean (takes ~ 1.2 mins)
hcSEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                innerLinkage = "single", distance = "euclidean",
                                seed = 123, verbose = T)

# HC Diana Euclidean (takes ~ 7.2 mins)
hcDianaEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                    clusterAlg = "dianaHook", distance = "euclidean",
                                    seed = 123, verbose = T)

# K-Means Euclidean (takes ~ 3 mins)
kmEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                               clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123, verbose = T)

# K-means Spearman (takes ~ 3 mins)
kmSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "kmdist", distance = "spearman",
                                seed = 123, verbose = T)

# K-means Mutual Information (takes ~ 3 mins)
kmMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                             clusterAlg = "kmdist", distance = "myMIdist",
                             seed = 123, verbose = T)

# PAM Euclidean (takes ~ 2.5 mins)
pamEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "pam", distance = "euclidean",
                                seed = 123, verbose = T)

# PAM Spearman (takes ~ 3 mins)
pamSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                 clusterAlg = "pam", distance = "spearman",
                                 seed = 123, verbose = T)

# PAM Mutual Information (takes ~ 2.5 mins)
pamMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                              clusterAlg = "pam", distance = "myMIdist",
                              seed = 123, verbose = T)

# Save ConsensusClusterPlus (CCP) results
saveRDS(list(hcAEucl, hcSEucl, hcDianaEucl, kmEucl, kmSpear,
             kmMI, pamEucl, pamSpear, pamMI),
        "TCGA/outputs/results_CCP.rds", compress = "xz")
