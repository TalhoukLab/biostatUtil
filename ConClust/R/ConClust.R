#' Consensus clustering over samples and algorithms
#'
#' Generates multiple runs for consensus clustering among replicated subsamples
#' of a dataset as well as across different clustering algorithms.
#'
#' @param x data matrix; genes are rows and samples are columns
#' @param k number of clusters requested
#' @param pItem proportion of items to be used in subsampling within an algorithm
#' @param reps number of subsamples
#' @param method vector of clustering algorithms for performing consensus clustering. Must be
#' any number of the following: "nmfDiv", "nmfEucl", "hcAEucl", "hcDianaEucl", "kmEucl",
#' "kmSpear", "kmMI", "pamEucl", "pamSpear", "pamMI".
#' @param seed random seed to use for NMF-based algorithms
#' @param dir directory where returned object will be saved at each iteration (as an RDS object).
#' No output file is saved if \code{file} is \code{NULL}.
#' @return An array of dimension \code{nrow(x)} by \code{reps} by \code{length(methods)}
#' Each slice of the array is a matrix showing consensus clustering results for
#' algorithms (method). The matrices have a row for each sample, and a column for each
#' subsample. Each entry represents a class membership.
#' @author Derek Chiu, Aline Talhouk
# ccc <- ConsensusCluster(TCGA.raw, pItem = 0.8, reps = 1000, k = 4, OF = "consensus_clustering/")
#' @importFrom magrittr set_rownames
#' @export
ConClust <- function(x, k, pItem = 0.8, reps = 1000, method = NULL,
                     seed = 123456, dir = NULL) {
  . <- NULL
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
  n.new <- floor(n * pItem)
  nm <- length(method)
  coclus <- array(NA, c(n, reps, nm),
                  dimnames = list(samples, paste0("R", 1:reps), method))
  pb <- txtProgressBar(min = 0, max = reps, style = 3)

  for (j in 1:nm) {
    for (i in 1:reps) {
      setTxtProgressBar(pb, i)
      ind.new <- sample(n, n.new, replace = F)
      x.nmf.samp <- x.nmf[!(apply(x.nmf[, ind.new], 1,
                                  function(x) all(x == 0))), ind.new]

      coclus[ind.new, i, j] <- switch(
        method[j],
        nmfDiv = predict(NMF::nmf(
          x.nmf.samp, rank = k, method = "brunet", seed = seed)),
        nmfEucl = predict(NMF::nmf(
          x.nmf.samp, rank = k, method = "lee", seed = seed)),
        hcAEucl = cutree(hclust(dist(t(x.rest[, ind.new])),
                                method = "average"), k),
        hcDianaEucl = cutree(diana(euc(t(x.rest[, ind.new])),
                                   diss = TRUE), k),
        kmEucl = kmeans(euc(t(x.rest[, ind.new])),
                        k)$cluster,
        kmSpear = kmeans(spearman.dist(t(x.rest[, ind.new])),
                         k)$cluster,
        kmMI = kmeans(myMIdist(t(x.rest[, ind.new])),
                      k)$cluster,
        pamEucl = pam(euc(t(x.rest[, ind.new])), k,
                      cluster.only = TRUE),
        pamSpear = pam(spearman.dist(t(x.rest[, ind.new])), k,
                       cluster.only = TRUE),
        pamMI = pam(myMIdist(x.rest[, ind.new]), k,
                    cluster.only = TRUE))
    }
  }
  if (!is.null(dir))
    saveRDS(coclus, paste0(dir, "ConClustOutput.rds"))
  return(coclus)
}
