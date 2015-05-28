source("consensus_clustering/functions/PAC.R")
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

# Cohen's kappa and Fleiss' kappa
wkappa(final.compare)

# CDF
test <- readRDS(file = "consensus_clustering/TCGA/outputs/nmf_output_05-19-2015.rds")
nmf.div <- consensusMatrix(dat = test[, , 1])
qplot(nmf.div[lower.tri(nmf.div)], stat = "ecdf", geom = "line")

qplot(km.euc[[4]]$consensusMatrix[lower.tri(km.euc[[4]]$consensusMatrix)], stat = "ecdf", geom = "step")

# PAC
nmf.div.PAC <- PAC(nmf.div)
km.euc.PAC <- PAC(km.euc[[4]]$consensusMatrix)
