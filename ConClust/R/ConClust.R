#' Consensus clustering over samples and algorithms
#'
#' Generates multiple runs for consensus clustering among replicated subsamples
#' of a dataset as well as across different clustering algorithms.
#'
#' The \code{min.sd} argument is used to filter the feature space for only highly variable
#' features. Only features with a standard deviation across all samples greater than
#' \code{min.sd} will be used.
#'
#' @param x data matrix; genes are rows and samples are columns
#' @param k number of clusters requested
#' @param pItem proportion of items to be used in subsampling within an algorithm
#' @param reps number of subsamples
#' @param method vector of clustering algorithms for performing consensus clustering. Must be
#' any number of the following: "nmfDiv", "nmfEucl", "hcAEucl", "hcDianaEucl", "kmEucl",
#' "kmSpear", "kmMI", "pamEucl", "pamSpear", "pamMI".
#' @param seed random seed to use for NMF-based algorithms
#' @param seed.method seed to use to ensure each method operates on the same set of subsamples
#' @param min.sd minimum standard deviation threshold. See details.
#' @param dir directory where returned object will be saved at each iteration (as an RDS object).
#' No output file is saved if \code{file} is \code{NULL}.
#' @param time.saved logical; if \code{TRUE} (default), the date saved is appended
#' to the file name. Only applicable when \code{dir} is not \code{NULL}.
#' @return An array of dimension \code{nrow(x)} by \code{reps} by \code{length(methods)}
#' Each slice of the array is a matrix showing consensus clustering results for
#' algorithms (method). The matrices have a row for each sample, and a column for each
#' subsample. Each entry represents a class membership.
#' @author Derek Chiu, Aline Talhouk
#' @importFrom magrittr extract
#' @export
ConClust <- function(x, k, pItem = 0.8, reps = 1000, method = NULL,
                     seed = 123456, seed.method = 1, min.sd = 1, dir = NULL,
                     time.saved = TRUE) {
  . <- NULL
  x.rest <- x %>%
    as.data.frame %>%
    select(which(sapply(., class) == "numeric")) %>%
    extract(apply(., 1, function(x) sd(x, na.rm = T)) > min.sd,
            apply(., 2, function(x) !any(is.na(x)))) %>%
    t %>%
    scale %>%
    t %>%
    as.data.frame

  x.nmf <- x.rest %>%
    rbind(-.) %>%
    apply(2, function(x) ifelse(x < 0, 0, x))

  samples <- colnames(x.rest)
  n <- ncol(x.rest)
  n.new <- floor(n * pItem)
  if (is.null(method))
    method <- c("nmfDiv", "nmfEucl", "hcAEucl", "hcDianaEucl", "kmEucl",
                "kmSpear", "kmMI", "pamEucl", "pamSpear", "pamMI", "apEucl")
  nm <- length(method)
  coclus <- array(NA, c(n, reps, nm),
                  dimnames = list(samples, paste0("R", 1:reps), method))
  pb <- txtProgressBar(min = 0, max = reps, style = 3)

  for (j in 1:nm) {
    set.seed(seed.method)
    for (i in 1:reps) {
      setTxtProgressBar(pb, i)
      ind.new <- sample(n, n.new, replace = F)
      if (any(c("nmfDiv", "nmfEucl") %in% method))
        x.nmf.samp <- x.nmf[!(apply(x.nmf[, ind.new], 1,
                                    function(x) all(x == 0))), ind.new]

      coclus[ind.new, i, j] <- switch(
        method[j],
        nmfDiv = NMF::predict(NMF::nmf(
          x.nmf.samp, rank = k, method = "brunet", seed = seed)),
        nmfEucl = NMF::predict(NMF::nmf(
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
        pamMI = pam(myMIdist(t(x.rest[, ind.new])), k,
                    cluster.only = TRUE),
        apEucl = setNames(dense_rank(apclusterK(negDistMat, dat, k)@idx),
                          rownames(dat)))
    }
  }
  if (!is.null(dir))
    if (time.saved)
      saveRDS(coclus, paste0(dir, "ConClustOutput_", Sys.Date(), ".rds"))
    else
      saveRDS(coclus, paste0(dir, "ConClustOutput.rds"))
  return(coclus)
}
