library(clv)
library(dplyr)
library(magrittr)

ccp.clust <- readRDS("consensus_clustering/TCGA/outputs/results_CCP.rds")
nmf.clust <- readRDS("consensus_clustering/TCGA/outputs/classes_NMF.rds")
all.clust <- ccp.clust %>%
  sapply(., function(x) x[[4]]$consensusClass) %>%
  cbind(., nmf.clust)

# Import
dat.raw <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/TCGA.csv")

# Remove genes with low variability and scale
dat <- dat.raw %>%
  set_rownames(.$UNIQID) %>%
  select(which(sapply(., class) == "numeric")) %>%
  extract(apply(., 1, sd) > 1, ) %>%
  t %>%
  scale

# Davies-Bouldin Index (lower the better)
DBI <- dat %>%
  apply(all.clust, 2, cls.scatt.data, data = .) %>%
  sapply(., clv.Davies.Bouldin, intracls = "average", intercls = "average") %>%
  sort

# Dunn Index (higher the better)
DI <- dat %>%
  apply(all.clust, 2, cls.scatt.data, data = .) %>%
  sapply(., clv.Dunn, intracls = "average", intercls = "average") %>%
  sort(decreasing = T)

# Silhouette Average Width (higher the better)
SAW <- apply(all.clust, 2, function(x) summary(silhouette(x, dist(dat)))$avg.width) %>% 
  sort(decreasing = T)
