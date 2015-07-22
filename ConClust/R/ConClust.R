#' Consensus clustering over samples and algorithms
#'
#' Generates multiple runs for consensus clustering among replicated subsamples
#' of a dataset as well as across different clustering algorithms.
#'
#' @param x data matrix; genes are rows and samples are columns
#' @param k number of clusters requested
#' @param pItem proportion of items to be used in subsampling within an algorithm
#' @param reps number of subsamples
#' @param file directory where returned object will be saved (at each iteration).
#' No output file is saved if \code{file} is \code{NULL}.
#' @return An array of dimension \code{nrow(x)} by \code{reps} by \code{length(methods)}
#' Each slice of the array is a matrix showing consensus clustering results for
#' algorithms (method). The matrices have a row for each sample, and a column for each
#' subsample. Each entry represents a class membership.
#' @author Derek Chiu, Aline Talhouk
#' @importFrom magrittr set_rownames
#' @export
ConClust <- function(x, k, pItem = 0.8, reps = 1000, file = NULL) {
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
  connect.matrix <- array(0, c(n, n, 10))
  coclus <- array(NA, c(n, reps, 10),
                  dimnames = list(samples, paste0("R", 1:reps),
                                  c("nmfDiv", "nmfEucl", "hcAEucl",
                                    "hcDianaEucl", "kmEucl", "kmSpear", "kmMI",
                                    "pamEucl", "pamSpear", "pamMI")))
  n.new <- floor(n * pItem)
  pb <- txtProgressBar(min = 0, max = reps, style = 3)


  for (i in 1:reps) {
    setTxtProgressBar(pb, i)
    ind.new <- sample(n, n.new, replace = F)
    # nmfDiv
    x.nmf.samp <- x.nmf[!(apply(x.nmf[, ind.new], 1, function(x) all(x == 0))),
                        ind.new]
    coclus[ind.new, i, 1] <- predict(NMF::nmf(x.nmf.samp, rank = k,
                                              method = "brunet", seed = 123456789))
    # nmfEucl
    coclus[ind.new, i, 2] <- predict(NMF::nmf(x.nmf.samp, rank = k,
                                              method = "lee", seed = 123456789))
    # hcAEucl
    coclus[ind.new, i, 3] <- cutree(hclust(dist(t(x.rest[, ind.new])),
                                           method = "average"), k)
    # hcDianaEucl
    coclus[ind.new, i, 4] <- cutree(diana(euc(t(x.rest[, ind.new])),
                                          diss = TRUE), k)
    # hcAgnesEucl
    coclus[ind.new, i, 5] <- cutree(agnes(euc(t(x.rest[, ind.new])),
                                          diss = TRUE), k)
    # kmeans Euclidean
    coclus[ind.new, i, 6] <- kmeans(euc(t(x.rest[, ind.new])),
                                    k)$cluster
    # kmeans Spearman
    coclus[ind.new, i, 7] <- kmeans(spearman.dist(t(x.rest[, ind.new])),
                                    k)$cluster
    # kmeans MI
    coclus[ind.new, i, 8] <- kmeans(myMIdist(t(x.rest[, ind.new])),
                                    k)$cluster
    # pamEucl
    coclus[ind.new, i, 9] <- pam(euc(t(x.rest[, ind.new])), k,
                                 cluster.only = TRUE)
    # pamSpear
    coclus[ind.new, i, 10] <- pam(spearman.dist(t(x.rest[, ind.new])), k,
                                  cluster.only = TRUE)
    # pamMI
    coclus[ind.new, i, 11] <- pam(myMIdist(x.rest[, ind.new]), k,
                                  cluster.only = TRUE)
  }
  if (!is.null(file))
    saveRDS(coclus, paste0(file, ".rds"))
  return(coclus)
}
