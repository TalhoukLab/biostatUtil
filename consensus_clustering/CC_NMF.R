# Load
library(NMF)
library(ConsensusClusterPlus)
library(dplyr)
library(magrittr)

# Import
TCGA.raw <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/TCGA.csv")
Tothill.raw <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/Tothill.csv")

# Transform raw data to NMF friendly form by dealing with negative entries
TCGA <- TCGA.raw %>%
  set_rownames(.$UNIQID) %>%
  select(-UNIQID) %>%
  t %>%
  scale %>%
  t %>%
  as.data.frame %>%
  rbind(-.) %>%
  apply(2, function(x) ifelse(x < 0, 0, x))

Tothill <- Tothill.raw %>%
  set_rownames(.$UNIQID) %>%
  select(-UNIQID, -NAME) %>%
  t %>%
  scale %>%
  t %>%
  as.data.frame %>%
  
# Apply NMF
TCGA.mod.brunet <- nmf(TCGA, 4, "brunet", .options = "t")
TCGA.mod.lee <- nmf(TCGA, 4, "lee", .options = "t")

Tothill.mod.brunet <- nmf(Tothill, 3, "brunet", .options = "t")
Tothill.mod.lee <- nmf(Tothill, 3, "lee", .options = "t")

mod <- TCGA.mod.brunet
H <- coef(mod)
assignment <- apply(H, 2, function(x) head(order(x), 1))
names(assignment) <- substring(text = names(assignment), first = 14, last = 14)
(compare.tab <- table(assignment, names(assignment)))

# Use consensus clustering
# nmf.clust <- function(dist, k) {
#   mod <- nmf(dist, k, "brunet", .options = "t")
#   H <- coef(mod)
#   assignment <- apply(H, 2, function(x) head(order(x), 1))
#   return(assignment)
# }
# 
# cc.nmf <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 2, pItem = 0.8, clusterAlg = "nmf.clust", distance = "pearson", seed = 123)

# user  system elapsed 
# 164.093  15.505 177.808 
cc.km.euc <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "km", distance = "euclidean",
                                seed = 123, verbose = T)

# user  system elapsed 
# 165.402  14.873 178.425 
cc.km.spr <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "km", distance = "spearman",
                           seed = 123, verbose = T)

# user  system elapsed 
# 156.912  17.560 172.940 
cc.km.min <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "km", distance = "minkowski",
                            seed = 123, verbose = T)

# user  system elapsed 
# 115.549  24.418 138.091 
cc.pam.euc <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "euclidean",
                            seed = 123, verbose = T)

# user  system elapsed 
# 136.183  23.754 158.044 
cc.pam.spr <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "spearman",
                            seed = 123, verbose = T)

# user  system elapsed 
# 122.436  23.794 144.373 
cc.pam.min <- ConsensusClusterPlus(TCGA, maxK = 4, reps = 1000, pItem = 0.8, clusterAlg = "pam", distance = "minkowski",
                            seed = 123, verbose = T)


# Probe to Gene annotation attempt
x <- hgu133plus2SYMBOL
mapped_probes <- mappedkeys(x)
lookup <- data.frame(gene = unlist(as.list(x[mapped_probes])))
mapped_genes <- match(Tothill.raw$UNIQID, row.names(lookup))
compare_map <- data.frame(raw = Tothill.raw$NAME, mapped = lookup[mapped_genes,]) %>%
  mutate(raw = as.character(raw),
         mapped = as.character(mapped),
         check = ifelse(raw != "---" | !is.na(mapped), "YES", "NO"))
sum(compare_map$check == "YES", na.rm = T)

# xx <- unlist(as.list(hgu133plus2ALIAS2PROBE))
# lookup <- data.frame(probe = xx, gene = names(xx))
# mapped_genes <- match(Tothill.raw$UNIQID, lookup$probe)
# compare_map <- data.frame(raw = Tothill.raw$NAME, mapped = lookup[mapped_genes, "gene"]) %>%
#   mutate(raw = as.character(raw),
#          mapped = as.character(mapped),
#          check = ifelse(raw != "---" | !is.na(mapped), "YES", "NO"))
# sum(compare_map$check == "YES", na.rm = T)
