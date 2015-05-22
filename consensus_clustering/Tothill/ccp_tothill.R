# Load
source("consensus_clustering/distance_functions.R")
library(ConsensusClusterPlus)
library(dplyr)
library(magrittr)

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

# user  system elapsed 
# 47.837  25.207  72.141 
hc.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, seed = 123, verbose = T)
hc.euc.tab <- hc.euc[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 405.932  28.070 432.125 
hc.diana <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "dianaHook", seed = 123, verbose = T)
hc.diana.tab <- hc.diana[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 315.106  27.552 340.693 
hc.agnes <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "agnesHook", seed = 123, verbose = T)
hc.agnes.tab <- cc.hc.agnes[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 164.093  15.505 177.808 
km.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123, verbose = T)
km.euc.tab <- km.euc[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 165.402  14.873 178.425 
km.spr <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "spearman",
                               seed = 123, verbose = T)
km.spr.tab <- km.spr[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 156.912  17.560 172.940 
km.min <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "myMIdist",
                               seed = 123, verbose = T)
km.min.tab <- km.min[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 115.549  24.418 138.091 
pam.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "euclidean",
                                seed = 123, verbose = T)
pam.euc.tab <- pam.euc[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 136.183  23.754 158.044 
pam.spr <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "spearman",
                                seed = 123, verbose = T)
pam.spr.tab <- pam.spr[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 122.436  23.794 144.373 
pam.min <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "myMIdist",
                                seed = 123, verbose = T)
pam.min.tab <- pam.min[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# Save results into an R object
all.tables <- list(hc.euc.tab, hc.diana.tab, hc.agnes.tab, km.euc.tab,
                   km.spr.tab, km.min.tab, pam.euc.tab, pam.spr.tab, pam.min.tab)
mets = c("hcAEucl","hcDianaEucl","hcAgnesEucl","kmEucl","kmSpear","kmMI","pamEucl","pamSpear","pamMI")
names(all.tables) <- mets
saveRDS(all.tables, "consensus_clustering/Tothill/CCP_output.rds")
