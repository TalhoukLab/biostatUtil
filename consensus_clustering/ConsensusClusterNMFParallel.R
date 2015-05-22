ConsensusClusterNMFParallel <- function(x, pItem, reps, k, OF, mets = c("nmfDiv","nmfEucl")) {
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
  require(foreach)
  require(doSNOW)
  require(doMC)
  
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
  
  registerDoMC(4)
  
  coclus[, , 1] <- foreach(i = 1:reps, .combine = 'cbind', .packages = 'NMF') %dopar% {
    ind.new <- sample(n, n.new, replace = F)
    nmf.x <- xr[!(apply(xr[, ind.new], 1, function(x) all(x == 0))), ind.new]
    coclus[ind.new, i, 1] <- predict(nmf(nmf.x, rank = k, method = "brunet", seed = 123456789))
    coclus[, , 1]
  } %>%
    extract(, seq(1, reps^2, reps + 1))
  
  coclus[, , 2] <- foreach(i = 1:reps, .combine = 'cbind', .packages = 'NMF') %dopar% {
    ind.new <- sample(n, n.new, replace = F)
    nmf.x <- xr[!(apply(xr[, ind.new], 1, function(x) all(x == 0))), ind.new]
    coclus[ind.new, i, 2] <- predict(nmf(nmf.x, rank = k, method = "lee", seed = 123456789))
    coclus[, , 2]
  } %>%
    extract(, seq(1, reps^2, reps + 1))
    
  saveRDS(coclus, paste0(OF, "nmf_output_", format(Sys.time(), "%m-%d-%Y"), ".rds"))
}


# Test 1000 reps for k = 4
x <- read.csv("~/Documents/Project 1 - HGSC Subtype/Datasets/TCGA.csv")
ConsensusClusterNMFParallel(x, pItem = 0.8, reps = 1000, k = 4, OF = "consensus_clustering/")

# Get consensus clusters
results.nmf <- readRDS("consensus_clustering/TCGA/nmf_output_05-19-2015.rds")

nmf.div <- results.nmf %>%
  extract(, , 1) %>%
  apply(., 1, function(x) names(which.max(table(x)))) %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))

nmf.eucl <- results.nmf %>%
  extract(, , 2) %>%
  apply(., 1, function(x) names(which.max(table(x)))) %>%
  set_names(substring(names(.), first = 18, last = 19)) %>%
  table(., names(.))
