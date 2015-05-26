# Load
source("consensus_clustering/functions/distance_functions.R")
library(ConsensusClusterPlus)
library(dplyr)
library(magrittr)
library(stringr)

# Import
dat.raw <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/Tothill.csv")

# Remove genes with low variability and scale
dat <- dat.raw %>%
  set_rownames(.$UNIQID) %>%
  select(which(sapply(., class) == "numeric")) %>%
  extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale %>%
  t

# Use consensus clustering

hc.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, seed = 123)
hc.euc.tab <- hc.euc[[4]]$consensusClass

hc.diana <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "dianaHook", seed = 123)
hc.diana.tab <- hc.diana[[4]]$consensusClass

km.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "euclidean", seed = 123)
km.euc.tab <- km.euc[[4]]$consensusClass

km.spr <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "spearman", seed = 123)
km.spr.tab <- km.spr[[4]]$consensusClass

km.min <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "myMIdist", seed = 123)
km.min.tab <- km.min[[4]]$consensusClass

pam.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "euclidean", seed = 123)
pam.euc.tab <- pam.euc[[4]]$consensusClass

pam.spr <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "spearman", seed = 123)
pam.spr.tab <- pam.spr[[4]]$consensusClass

pam.min <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "myMIdist", seed = 123)
pam.min.tab <- pam.min[[4]]$consensusClass

# Save results into an R object
cbind(hc.euc.tab, hc.diana.tab, km.euc.tab, km.spr.tab,
      km.min.tab, pam.euc.tab, pam.spr.tab, pam.min.tab) %>%
  set_colnames(c("hcAEucl","hcDianaEucl","kmEucl","kmSpear","kmMI","pamEucl","pamSpear","pamMI")) %>%
  saveRDS("consensus_clustering/Tothill/outputs/CCP_classes.rds")
