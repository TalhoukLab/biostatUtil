library(mclust)
library(entropy)
library(caret)
library(infotheo)

CCP.clusts <- readRDS("consensus_clustering/TCGA/outputs/classes_CCP.rds")
NMF.clusts <- readRDS("consensus_clustering/TCGA/outputs/classes_NMF.rds")
all.clust <- cbind(CCP.clusts, NMF.clusts) %>%
  set_rownames(substring(rownames(.), first = 18))
meta.clusts <- consensusMatrix(all.clusts)

# Confusion Matrix
final.compare <- hclust(dist(meta.clusts), method = "average") %>%
  cutree(4) %>%
  as.factor %>%
  set_names(hclust(dist(meta.clusts), method = "average")$labels) %>%
  set_names(substring(names(.), first = 18)) %>%
  table(., names(.)) %>%
  extract(names(sort(apply(., 1, which.max))), ) %>%
  set_rownames(colnames(.)) %>%
  as.table

names(dimnames(final.compare)) <- c("Predicted", "Reference")
confusionMatrix(final.compare)

# Adjusted Rand Index
sort(apply(all.clust, 2, adjustedRandIndex, x = rownames(all.clust)))

# Mutual Information
sort(apply(all.clust, 2, mutinformation, Y = rownames(all.clust)))
mi.plugin(final.compare)
