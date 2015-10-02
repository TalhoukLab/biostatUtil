#' Combine the consensus results from all algorithms
#' 
#' The function can output either the consensus matrices
#' or consensus classes for all algorithms
#' 
#' @param ... any number of objects outputted from \code{ConClust}
#' @param res.CCP an object outputted from \code{ConClustPlus}
#' @param k desired number of clusters
#' @param element the element to extract. This is either "matrix" or "class" for
#' consensus matrix or consensus classes, respectively.
#' @param alg.names optional. Supply a vector of names for the algorithms.
#' @return Either a list of all consensus matrices or a data frame
#' showing all the consensus classes
#' @author Derek Chiu
#' @export
combineAlgs <- function(..., res.CCP, k, element, alg.names = NULL) {
  obj <- unlist(list(...), recursive = FALSE)
  if (element == "matrix") {
    out.CC <- lapply(obj, "[[", "consensusMatrix")
    out.CCP <- lapply(lapply(res.CCP, "[[", k), "[[", "consensusMatrix")
    out.CCP <- lapply(out.CCP, function(x) {
      dimnames(x) <- dimnames(out.CC[[1]])
      return(x)
    })
    out <- unlist(list(out.CCP, out.CC), recursive = FALSE)
    if (!is.null(alg.names))
      names(out) <- alg.names
  } else if (element == "class") {
    out.CC <- sapply(obj, "[[", "consensusClass")
    out.CCP <- sapply(lapply(res.CCP, "[[", k), "[[", "consensusClass")
    out <- as.matrix(data.frame(out.CCP, out.CC))
    if (!is.null(alg.names))
      colnames(out) <- alg.names
  } else {
    stop('The argument element must be one of "matrix" or "class".')
  }
  return(out)
}