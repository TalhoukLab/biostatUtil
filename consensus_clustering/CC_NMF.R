# Load
source("consensus_clustering/distance_functions.R")
library(NMF)
library(ConsensusClusterPlus)
library(dplyr)
library(magrittr)

# Import
TCGA.raw <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/TCGA.csv")
Tothill.raw <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/Tothill.csv")

# Transform raw data to NMF friendly form by dealing with negative entries
TCGA <- TCGA.raw %>%
  set_rownames(.$UNIQID) %>%
  select(which(sapply(., class) == "numeric")) %>%
  extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale %>%
  t #%>%
#   as.data.frame %>%
#   rbind(-.) %>%
#   apply(2, function(x) ifelse(x < 0, 0, x))

Tothill <- Tothill.raw %>%
  set_rownames(.$UNIQID) %>%
  select(which(sapply(., class) == "numeric")) %>%
  extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale %>%
  t %>%
  as.data.frame %>%
  rbind(-.) %>%
  apply(2, function(x) ifelse(x < 0, 0, x))

# Use consensus clustering

# user  system elapsed 
# 47.837  25.207  72.141 
hc.euc <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, seed = 123, verbose = T)
hc.euc.tab <- hc.euc[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 405.932  28.070 432.125 
hc.diana <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "dianaHook", seed = 123, verbose = T)
hc.diana.tab <- hc.diana[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 315.106  27.552 340.693 
hc.agnes <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "agnesHook", seed = 123, verbose = T)
hc.agnes.tab <- cc.hc.agnes[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 164.093  15.505 177.808 
km.euc <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "euclidean",
                               seed = 123, verbose = T)
km.euc.tab <- km.euc[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 165.402  14.873 178.425 
km.spr <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "spearman",
                                  seed = 123, verbose = T)
km.spr.tab <- km.spr[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 156.912  17.560 172.940 
km.min <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "kmdist", distance = "myMIdist",
                                  seed = 123, verbose = T)
km.min.tab <- km.min[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 115.549  24.418 138.091 
pam.euc <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "euclidean",
                                   seed = 123, verbose = T)
pam.euc.tab <- pam.euc[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 136.183  23.754 158.044 
pam.spr <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "spearman",
                                   seed = 123, verbose = T)
pam.spr.tab <- pam.spr[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# user  system elapsed 
# 122.436  23.794 144.373 
pam.min <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "myMIdist",
                                   seed = 123, verbose = T)
pam.min.tab <- pam.min[[4]]$consensusClass %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

# NMF
# TCGA.mod.brunet <- nmf(TCGA, rank = 4, "brunet", .options = "t")
# TCGA.mod.lee <- nmf(TCGA, rank = 4, "lee", .options = "t")
# 
# Tothill.mod.brunet <- nmf(Tothill, rank = 3, "brunet", .options = "t")
# Tothill.mod.lee <- nmf(Tothill, rank = 3, "lee", .options = "t")

# Probe to Gene annotation attempt
x <- hgu133plus2SYMBOL
mapped_probes <- mappedkeys(x)
lookup <- data.frame(gene = unlist(as.list(x[mapped_probes])))
mapped_genes <- match(Tothill.raw$UNIQID, row.names(lookup))
compare_map <- data.frame(raw = Tothill.raw$NAME, mapped = lookup[mapped_genes,]) %>%
  mutate(raw = as.character(raw),
         mapped = as.character(mapped),
         check = ifelse(raw != "---" | !is.na(mapped), "YES", "NO"))
sum(compare_map$check == "YES", na.rm = T)

# xx <- unlist(as.list(hgu133plus2ALIAS2PROBE))
# lookup <- data.frame(probe = xx, gene = names(xx))
# mapped_genes <- match(Tothill.raw$UNIQID, lookup$probe)
# compare_map <- data.frame(raw = Tothill.raw$NAME, mapped = lookup[mapped_genes, "gene"]) %>%
#   mutate(raw = as.character(raw),
#          mapped = as.character(mapped),
#          check = ifelse(raw != "---" | !is.na(mapped), "YES", "NO"))
# sum(compare_map$check == "YES", na.rm = T)
