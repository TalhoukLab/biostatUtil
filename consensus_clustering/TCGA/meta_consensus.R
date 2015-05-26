source("consensus_clustering/consensusMatrix.R")

CCP.clusts <- readRDS("consensus_clustering/TCGA/CCP_output.rds")
NMF.clusts <- readRDS("consensus_clustering/TCGA/nmf_clust.RDS")
all.clusts <- cbind(CCP.clusts, NMF.clusts)

meta.clusts <- consensusMatrix(all.clusts)

library(RColorBrewer)
BuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
palBuPu <- BuPuFun(paletteSize)

# Heatmaps
heatmap(meta.clusts, labRow = NA, labCol = NA, col = palBuPu)

# Compare final clustering with TCGA
final.compare <- hclust(dist(meta.clusts), method = "average") %>%
  cutree(4) %>%
  as.factor %>%
  set_names(hclust(dist(meta.clusts), method = "average")$labels) %>%
  set_names(substring(names(.), first = 18)) %>%
  table(., names(.))
  
