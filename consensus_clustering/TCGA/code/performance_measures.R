library(mclust)
library(caret)
library(ROCR)

CCP.clusts <- readRDS("consensus_clustering/TCGA/outputs/classes_CCP.rds")
NMF.clusts <- readRDS("consensus_clustering/TCGA/outputs/classes_NMF.rds")
all.clust <- cbind(CCP.clusts, NMF.clusts) %>%
  set_rownames(substring(rownames(.), first = 18))

# Adjusted Rand Index
sort(apply(all.clust, 2, adjustedRandIndex, x = rownames(all.clust)))

# 
# hc.final <- hclust(dist(meta.clusts), method = "average")
# prediction(predictions = cutree(hc.final, 4), labels = substring(hc.final$labels, first = 18))
