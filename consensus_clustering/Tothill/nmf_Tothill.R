source("consensus_clustering/ConsensusClusterNMFParallel.R")
x <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/Tothill.csv")
ConsensusClusterNMFParallel(x, pItem = 0.8, reps = 1000, k = 4,
                            OF = "consensus_clustering/Tothill/")

# Get consensus clusters
results.nmf <- readRDS(paste0("consensus_clustering/Tothill/nmf_output_",
                              format(Sys.time(), "%m-%d-%Y"), ".rds"))

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
