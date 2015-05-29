source("consensus_clustering/functions/PAC.R")
library(mclust)
library(entropy)
library(caret)
library(infotheo)

all.clust <- readRDS("consensus_clustering/TCGA/outputs/results_CCP.rds") %>%
  sapply(., function(x) x[[4]]$consensusClass) %>%
  cbind(., readRDS("consensus_clustering/TCGA/outputs/classes_NMF.rds"))
meta.cm <- consensusMatrix(all.clust)

# Confusion Matrix
final.compare <- hclust(dist(meta.cm), method = "average") %>%
  cutree(4) %>%
  as.factor %>%
  set_names(hclust(dist(meta.cm), method = "average")$labels) %>%
  set_names(substring(names(.), first = 18)) %>%
  table(., names(.)) %>%
  extract(names(sort(apply(., 1, which.max))), ) %>%
  set_rownames(colnames(.)) %>%
  as.table

names(dimnames(final.compare)) <- c("Predicted", "Reference")
confusionMatrix(final.compare)

# Adjusted Rand Index
sort(apply(all.clust, 2, adjustedRandIndex, x = substring(rownames(all.clust), first = 18)))

# Mutual Information
sort(apply(all.clust, 2, mutinformation, Y = substring(rownames(all.clust), first = 18)))
mi.plugin(final.compare)

# Fleiss' kappa: unweighted/weighted
wkappa(final.compare)

# CDF
test <- readRDS(file = "consensus_clustering/TCGA/outputs/nmf_output_05-19-2015.rds")
nmf.div <- consensusMatrix(dat = test[, , 1])
nmf.eucl <- consensusMatrix(dat = test[, , 2])
qplot(nmf.div[lower.tri(nmf.div)], stat = "ecdf", geom = "line")

qplot(km.euc[[4]]$consensusMatrix[lower.tri(km.euc[[4]]$consensusMatrix)], stat = "ecdf", geom = "step")

# PAC
PAC(nmf.div)
PAC(nmf.eucl)

PAC.all <- readRDS("consensus_clustering/TCGA/outputs/results_CCP.rds") %>%
  sapply(., function(x) PAC(x[[4]]$consensusMatrix)) %>%
  sort
