#' Combine consensus results
#'
#' Combine results from ConClust and ConClustPlus and output either the
#' consensus matrices or consensus classes for all algorithms from both objects.
#'
#' @param ... any number of objects outputted from \code{\link{ConClust}}
#' @param res.CCP an object outputted from \code{\link{ConClustPlus}}
#' @param k desired number of clusters
#' @param element either "matrix" or "class" to extract the consensus matrix or
#'   consensus class, respectively.
#' @param alg.names optional. Supply a vector of names for the algorithms.
#' @return Either a list of all consensus matrices or a data frame showing all
#'   the consensus classes
#' @author Derek Chiu
#' @export
#' @examples
#' set.seed(911)
#' x <- matrix(rnorm(1000), nrow = 10)
#' CC1 <- ConClust(x, k = 4, reps = 10, method = "apEucl", save = FALSE)
#' CC2 <- ConClust(x, k = 4, reps = 10, method = "gmmBIC", save = FALSE)
#' CCP <- ConClustPlus(x, k = 4, reps = 10, save = FALSE)
#' y1 <- combineAlgs(CC1, CC2, res.CCP = CCP, k = 4, element = "matrix")
#' str(y1)
#' y2 <- combineAlgs(CC1, CC2, res.CCP = CCP, k = 4, element = "class")
#' str(y2)
consensus_combine <- function(..., res.CCP, k, element = c("matrix", "class"),
                              alg.names = NULL) {
  obj <- list(...)
  names(obj) <- sapply(obj, function(x) unlist(attr(x, "dimnames")[3]))
  switch(match.arg(element),
         matrix = {
           out.CC <- lapply(obj, consensus_matrix)
           out.CCP <- lapply(lapply(res.CCP, "[[", k), "[[", "consensusMatrix")
           out.CCP <- lapply(out.CCP, function(x) {
             dimnames(x) <- dimnames(out.CC[[1]])
             return(x)
           })
           out <- unlist(list(out.CCP, out.CC), recursive = FALSE)
           if (!is.null(alg.names))
             names(out) <- alg.names
         },
         class = {
           out.CC <- lapply(obj, function(x)
             consensus_class(consensus_matrix(x), k))
           out.CCP <- sapply(lapply(res.CCP, "[[", k), "[[", "consensusClass")
           out <- as.matrix(data.frame(out.CCP, out.CC))
           if (!is.null(alg.names))
             colnames(out) <- alg.names
         })
  return(out)
}
