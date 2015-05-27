source("consensus_clustering/functions/consensusMatrix.R")

CCP.clusts <- readRDS("consensus_clustering/TCGA/outputs/classes_CCP.rds")
NMF.clusts <- readRDS("consensus_clustering/TCGA/outputs/classes_NMF.rds")
all.clusts <- cbind(CCP.clusts, NMF.clusts)
meta.clusts <- consensusMatrix(all.clusts)

# Heatmaps
library(RColorBrewer)
BuPuFun <- colorRampPalette(brewer.pal(n = 9, "BuPu"))
paletteSize <- 256
palBuPu <- BuPuFun(paletteSize)
heatmap(meta.clusts, labRow = NA, labCol = NA, col = palBuPu)

# Proportion at least 0.6
high.pairs <- (sum(meta.clusts >= 0.6) - ncol(meta.clusts)) / 2
total.pairs <- choose(ncol(meta.clusts), 2)
high.pairs / total.pairs
