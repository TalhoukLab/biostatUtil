#' Run a series of ConsensusClusterPlus algorithms
#'
#' Runs consensus clustering for multiple algorithms and saves the output
#'
#' @param dat data matrix; columns are samples and rows are genes/features
#' @param k maximum number of clusters to compute consensus results
#' @param reps number of subsamples
#' @param pItem proportion of features to use in each subsample
#' @param dir directory to save results
#' @param fileName file name of the written object
#' @param seed random seed to maintain reproducible results
#' @param min.sd minimum standard deviation across each gene
#' @param ... additional arguments to \code{ConsensusClusterPlus}
#' @return A list of outputs from \code{ConsensusClusterPlus}; each element
#' is for a different algorithm.
#' @author Derek Chiu
#' @import ConsensusClusterPlus
#' @export
#' @examples
#' set.seed(23)
#' ConClustPlus(matrix(rnorm(100), nrow = 10), k = 3, reps = 10, pItem = 0.9)
ConClustPlus <- function(dat, k = 3, reps = 1000, pItem = 0.8, dir = NULL,
                         fileName = "results_CCP", seed = 123, min.sd = 1,
                         ...) {
  dat <- dataPrep(dat, min.sd = min.sd)
  hcAEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                  distance = "euclidean",
                                  seed = seed, ...)
  hcSEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                  innerLinkage = "single",
                                  distance = "euclidean", seed = seed, ...)
  hcDianaEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps,
                                      pItem = pItem, clusterAlg = "dianaHook",
                                      distance = "euclidean", seed = seed, ...)
  kmEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                 clusterAlg = "kmdist", distance = "euclidean",
                                 seed = seed, ...)
  kmSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                  clusterAlg = "kmdist", distance = "spearman",
                                  seed = seed, ...)
  kmMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                               clusterAlg = "kmdist", distance = "myMIdist",
                               seed = seed, ...)
  pamEucl <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                  clusterAlg = "pam", distance = "euclidean",
                                  seed = seed, ...)
  pamSpear <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                   clusterAlg = "pam", distance = "spearman",
                                   seed = seed, ...)
  pamMI <- ConsensusClusterPlus(dat, maxK = k, reps = reps, pItem = pItem,
                                clusterAlg = "pam", distance = "myMIdist",
                                seed = seed, ...)
  results <- list(hcAEucl = hcAEucl, hcSEucl = hcSEucl,
                  hcDianaEucl = hcDianaEucl, kmEucl = kmEucl,
                  kmSpear = kmSpear, kmMI = kmMI, pamEucl = pamEucl,
                  pamSpear = pamSpear, pamMI = pamMI)
  if (!is.null(dir))
    saveRDS(results, paste0(dir, fileName, ".rds"), compress = "xz")
  return(results)
}
