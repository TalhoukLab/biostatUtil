source("consensus_clustering/ConsensusClusterNMFParallel.R")
source("consensus_clustering/functions/consensusMatrix.R")

# Get consensus clusters
x <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/TCGA.csv")
results.nmf <- readRDS("consensus_clustering/TCGA/outputs/nmf_output_05-19-2015.rds")

# Takes about ~ 3 mins to run each
nmf.div <- consensusMatrix(results.nmf[, , 1])
nmf.eucl <- consensusMatrix(results.nmf[, , 2])

# Get nice colours
library(RColorBrewer)
BuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
palBuPu <- BuPuFun(paletteSize)

# Heatmaps
heatmap(nmf.div, labRow = NA, labCol = NA, col = palBuPu)
heatmap(nmf.eucl, labRow = NA, labCol = NA, col = palBuPu)

# Hierarchical Clustering of consensus matrix
div.clust <- hclust(dist(nmf.div), method = "average") %>%
  cutree(4) %>%
  as.factor %>%
  set_names(hclust(dist(nmf.div), method = "average")$labels)

eucl.clust <- hclust(dist(nmf.eucl), method = "average") %>%
  cutree(4) %>%
  as.factor %>%
  set_names(hclust(dist(nmf.eucl), method = "average")$labels)

# Save clusters
cbind(div.clust, eucl.clust) %>%
  set_names(c("nmfDiv", "nmfEucl")) %>%
  saveRDS("consensus_clustering/TCGA/nmf_clust.rds")

