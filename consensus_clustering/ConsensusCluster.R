source("consensus_clustering/distance_functions.R")

ConsensusCluster <- function(x, pItem, reps, k, OF) {
  # Function that generates multiple runs for consensusClustering (among samples)
  # x: data matrix (genes are rows and samples are columns)
  # pItem: proportion of items to be used in the subsampling
  # reps: number of repetitions to be done
  # k: number of clusters requested
  # OF: directory where Output File will be written (at each iteration)
  
  # load required packages
  require(dplyr)
  require(magrittr)
  require(cluster)
  require(bioDist)
  require(RColorBrewer)
  require(NMF)
  
  # Remove genes with low signal and scale for rest of methods
  x.rest <- x %>%
    set_rownames(.$UNIQID) %>%
    select(which(sapply(., class) == "numeric")) %>%
    extract(apply(., 1, sd) > 1, ) %>%
    t %>%
    scale %>%
    t %>%
    as.data.frame

  # Deal with negative entries
  x.nmf <- x.rest %>%
    rbind(-.) %>%
    apply(2, function(x) ifelse(x < 0, 0, x))
  
  # Initialize empty arrays
  samples <- colnames(x.rest)
  genes <- rownames(x.rest)
  n <- ncol(x.rest)
  connect.matrix <- array(0, c(n, n, 11))
  coclus <- array(NA, c(n, reps, 11),
                  dimnames = list(samples, paste0("R", 1:reps),
                                  c("nmfDiv", "nmfEucl", "hcAEucl", "hcDianaEucl", "hcAgnesEucl",
                                    "kmEucl", "kmSpear", "kmMI", "pamEucl", "pamSpear", "pamMI")))
  
  # rows are samples
  # columns are Reps
  # 3rd D is method
  
  n.new <- floor(n * pItem)  # subset the data
  pb <- txtProgressBar(min = 0, max = reps, style = 3)

  
  for (i in 1:reps) {
    setTxtProgressBar(pb, i)
    ind.new <- sample(n, n.new, replace = F)
#       nmfDiv
#       x.nmf.samp <- x.nmf[!(apply(x.nmf[, ind.new], 1, function(x) all(x == 0))), ind.new]
#       coclus[ind.new, i, 1] <- predict(nmf(x.nmf.samp, rank = k, method = "brunet", seed = 123456789))
#       nmfEucl
#       coclus[ind.new, i, 2] <- predict(nmf(x.nmf.samp, rank = k, method = "lee", seed = 123456789))
#       hcAEucl
#       coclus[ind.new, i, 3] <- cutree(hclust(dist(t(x.rest[, ind.new])), method = "average"), k)
#       hcDianaEucl
#       coclus[ind.new, i, 4] <- cutree(diana(euc(t(x.rest[, ind.new])), diss = TRUE), k)
#       hcAgnesEucl
#       coclus[ind.new, i, 5] <- cutree(agnes(euc(t(x.rest[, ind.new])), diss = TRUE), k)
#       kmeans Euclidean
#       coclus[ind.new, i, 6] <- kmeans(euc(t(x.rest[, ind.new])), k)$cluster
#       kmeans Spearman
#       coclus[ind.new, i, 7] <- kmeans(spearman.dist(t(x.rest[, ind.new])), k)$cluster
#       kmeans MI
#       coclus[ind.new, i, 8] <- kmeans(myMIdist(x.rest[, ind.new]), k)$cluster
#       pamEucl
#       coclus[ind.new, i, 9] <- pam(euc(t(x.rest[, ind.new])), k, cluster.only = TRUE)
#       pamSpear
#       coclus[ind.new, i, 10] <- pam(spearman.dist(t(x.rest[, ind.new])), k, cluster.only = TRUE)
#       pamMI
#       coclus[ind.new, i, 11] <- pam(myMIdist(x.rest[, ind.new]), k, cluster.only = TRUE)
  }
#   saveRDS(coclus, paste0(OF, "test.rds"))
return(coclus)
}

ccc <- ConsensusCluster(TCGA.raw, pItem = 0.8, reps = 1000, k = 4, OF = "consensus_clustering/")

slice <- 10
test.class <- ccc %>%
  extract(, , slice) %>%
  apply(., 1, function(x) names(which.max(table(x)))) %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))
