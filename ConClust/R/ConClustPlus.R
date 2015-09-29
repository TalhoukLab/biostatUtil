#' Run a series of ConsensusClusterPlus algorithms
#' 
#' Runs consensus clustering for multiple algorithms and saves the output
#' 
#' @param dat data matrix; columns are samples and rows are genes/features
#' @param k maximum number of clusters to compute consensus results
#' @param reps number of subsamples
#' @param pItem proportion of features to use in each subsample
#' @param dir directory to save results
#' @param seed random seed to maintain reproducible results
#' @param ... additional arguments to \code{ConsensusClusterPlus}
#' @return A list of outputs from \code{ConsensusClusterPlus}; each element
#' is for a different algorithm.
#' @import ConsensusClusterPlus
#' @export
#' @examples 
#' set.seed(23)
#' ConClustPlus(matrix(rnorm(100), nrow = 10), k = 3, reps = 10, pItem = 0.9)
ConClustPlus <- function(dat, k = 3, reps = 1000, pItem = 0.8, dir = NULL, 
                         seed = 123, ...) {
  dat <- dataPrep(dat)
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
  results <- list(hcAEucl, hcSEucl, hcDianaEucl, kmEucl, kmSpear,
                  kmMI, pamEucl, pamSpear, pamMI)
  if (!is.null(dir))
    saveRDS(results, paste0(dir, "results_CCP.rds"), compress = "xz")
  return(results)
}