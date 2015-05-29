source("consensus_clustering/functions/PAC.R")
library(mclust)
library(entropy)
library(caret)
library(infotheo)
library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)

# Load results and classes objects
nmf.output <- readRDS("consensus_clustering/TCGA/outputs/
                      nmf_output_05-19-2015.rds")
ccp.clust <- readRDS("consensus_clustering/TCGA/outputs/results_CCP.rds")
nmf.clust <- readRDS("consensus_clustering/TCGA/outputs/classes_NMF.rds")
all.clust <- ccp.clust %>%
  sapply(., function(x) x[[4]]$consensusClass) %>%
  cbind(., nmf.clust)
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
sort(apply(all.clust, 2, adjustedRandIndex, x = substring(rownames(all.clust),
                                                          first = 18)))

# Mutual Information
sort(apply(all.clust, 2, mutinformation, Y = substring(rownames(all.clust),
                                                       first = 18)))
mi.plugin(final.compare)

# Fleiss' kappa: unweighted/weighted
wkappa(final.compare)

# CDF
nmf.div <- consensusMatrix(nmf.output[, , 1])
nmf.eucl <- consensusMatrix(nmf.output[, , 2])

CDF.all <- list(nmfDiv = nmf.div, nmfEucl = nmf.eucl) %>%
  ldply(.fun = function(x) x[lower.tri(x)]) %>%
  rbind(., ldply(ccp.clust, function(x) 
    x[[4]]$consensusMatrix[lower.tri(x[[4]]$consensusMatrix)])) %>%
  as.data.frame %>%
  set_rownames(.$.id) %>%
  select(-.id) %>%
  t %>%
  as.data.frame %>%
  gather(key = Method, value = CDF, 1:ncol(.)) 

ggplot(CDF.all, aes(x = CDF, colour = Method)) +
  stat_ecdf() +
  facet_wrap(~ Method)

# PAC
PAC.all <- ccp.clust %>%
  sapply(., function(x) PAC(x[[4]]$consensusMatrix)) %>%
  c(nmfDiv = PAC(nmf.div), nmfEucl = PAC(nmf.eucl)) %>%
  sort
