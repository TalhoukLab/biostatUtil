# Load
source("consensus_clustering/functions/distance_functions.R")
library(ConsensusClusterPlus)
library(dplyr)
library(magrittr)

# Import
dat.raw <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/TCGA.csv")

# Remove genes with low variability and scale
dat <- dat.raw %>%
  set_rownames(.$UNIQID) %>%
  select(which(sapply(., class) == "numeric")) %>%
  extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale %>%
  t

# Use consensus clustering

# user  system elapsed 
# 47.837  25.207  72.141 
hc.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, seed = 123, verbose = T)

# user  system elapsed 
# 405.932  28.070 432.125 
hc.diana <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "dianaHook", seed = 123, verbose = T)

# user  system elapsed 
# 164.093  15.505 177.808 
km.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123, verbose = T)
# km.euc.tab <- km.euc[[4]]$consensusClass

# user  system elapsed 
# 165.402  14.873 178.425 
km.spr <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "spearman",
                                  seed = 123, verbose = T)

# user  system elapsed 
# 156.912  17.560 172.940 
km.min <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "myMIdist",
                                  seed = 123, verbose = T)

# user  system elapsed 
# 115.549  24.418 138.091 
pam.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "euclidean",
                                   seed = 123, verbose = T)

# user  system elapsed 
# 136.183  23.754 158.044 
pam.spr <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "spearman",
                                   seed = 123, verbose = T)

# user  system elapsed 
# 122.436  23.794 144.373 
pam.min <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "myMIdist",
                                   seed = 123, verbose = T)

# Save results
list(hc.euc, hc.diana, km.euc, km.spr, km.min, pam.euc, pam.spr, pam.min) %>%
  set_names(c("hcAEucl", "hcDianaEucl","kmEucl", "kmSpear", "kmMI",
              "pamEucl", "pamSpear", "pamMI")) %>%
  saveRDS("consensus_clustering/TCGA/outputs/results_CCP.rds")
