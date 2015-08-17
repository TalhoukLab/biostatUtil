library(entropy)
library(caret)
library(ggplot2)
library(plyr)
library(tidyr)
library(magrittr)
library(psych)

# Proportion at least 0.6
high.pairs <- (sum(meta.cm >= 0.6) - ncol(meta.cm)) / 2
total.pairs <- choose(ncol(meta.cm), 2)
high.pairs / total.pairs

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

# Mutual Information
mi.plugin(final.compare)

# Fleiss' kappa: unweighted/weighted
wkappa(final.compare)
