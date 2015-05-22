# Load
source("consensus_clustering/distance_functions.R")
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
hc.euc.tab <- hc.euc %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

hc.diana <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "dianaHook", seed = 123)
hc.diana.tab <- hc.diana %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

hc.agnes <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "agnesHook", seed = 123)
hc.agnes.tab <- hc.agnes %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

km.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "euclidean", seed = 123)
km.euc.tab <- km.euc %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

km.spr <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "spearman", seed = 123)
km.spr.tab <- km.spr %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

km.min <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "myMIdist", seed = 123)
km.min.tab <- km.min %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

pam.euc <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "euclidean", seed = 123)
pam.euc.tab <- pam.euc %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

pam.spr <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "spearman", seed = 123)
pam.spr.tab <- pam.spr %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

pam.min <- ConsensusClusterPlus(dat, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "myMIdist", seed = 123)
pam.min.tab <- pam.min %>% 
  extract2(4) %>% 
  use_series(consensusClass) %>%
  set_names(str_sub(names(.), str_locate(names(.), "C")[, 1], str_locate(names(.), "C")[, 1] + 1)) %>%
  table(., names(.))

# Save results into an R object
all.tables <- list(hc.euc.tab, hc.diana.tab, hc.agnes.tab, km.euc.tab,
                   km.spr.tab, km.min.tab, pam.euc.tab, pam.spr.tab, pam.min.tab)
mets = c("hcAEucl","hcDianaEucl","hcAgnesEucl","kmEucl","kmSpear","kmMI","pamEucl","pamSpear","pamMI")
names(all.tables) <- mets
saveRDS(all.tables, "consensus_clustering/Tothill/CCP_output.rds")
