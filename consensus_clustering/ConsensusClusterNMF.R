ConsensusClusterNMF <- function(x, pItem, reps, k, OF, mets = c("nmfDiv","nmfEucl")) {
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
  
  # Remove genes with low signal, scale, and deal with negative entries
  xr <- x %>%
    set_rownames(.$UNIQID) %>%
    select(which(sapply(., class) == "numeric")) %>%
    extract(apply(., 1, sd) > 1, ) %>%
    t %>%
    scale %>%
    t %>%
    as.data.frame %>%
    rbind(-.) %>%
    apply(2, function(x) ifelse(x < 0, 0, x))
  
  samples <- colnames(xr)
  genes <- rownames(xr)
  
  m <- length(mets)
  n <- ncol(xr)
  connect.matrix <- array(0, c(n, n, m))
  coclus <- array(NA, c(n, reps, m), dimnames = list(samples, paste("R", 1:reps, sep = ""), mets))
  
  # rows are samples
  # columns are reps
  # 3rd D is method
  
  n.new <- floor(n * pItem)  # subset the data
  pb <- txtProgressBar(min = 0, max = reps, style = 3)
  
  for (i in 1:reps) {
    setTxtProgressBar(pb, i)
    ind.new <- sample(n, n.new, replace = F)
    # nmfDiv
    nmf.x <- xr[!(apply(xr[, ind.new], 1, function(x) all(x == 0))), ind.new]
    coclus[ind.new, i, 1] <- predict(nmf(nmf.x, rank = k, method = "brunet", seed = 123456789))
    # nmfEucl
    coclus[ind.new, i, 2] <- predict(nmf(nmf.x, rank = k, method = "lee", seed = 123456789))
    saveRDS(coclus, paste0(OF, "nmf_output_", format(Sys.time(), "%m-%d-%Y"), ".rds"))
  }
}


# Test 1000 reps for k = 4
x <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/TCGA.csv")
ConsensusClusterNMF(x, pItem = 0.8, reps = 1000, k = 4, OF = "consensus_clustering/")

# Get consensus clusters
results.nmf <- readRDS("consensus_clustering/outputMay19.rds")

nmfDiv.class <- results.nmf %>%
  extract(, , 1) %>%
  apply(., 1, function(x) names(which.max(table(x)))) %>%
  set_names(substring(names(.), first = 18, last = 19))
table(nmfDiv.class, names(nmfDiv.class))

nmfEucl.class <- results.nmf %>%
  extract(, , 2) %>%
  apply(., 1, function(x) names(which.max(table(x)))) %>%
  set_names(substring(names(.), first = 18, last = 19))
table(nmfEucl.class, names(nmfEucl.class))
