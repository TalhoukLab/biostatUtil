#' Obtain consensus classes
#'
#' Performs hierarchical clustering on a consensus matrix to obtain
#' consensus class labels.
#'
#' @param x a consensus matrix
#' @param k desired number of clusters
#' @param method linkage type for hierarchical clustering
#' @param names vector of names for each sample. Default is \code{NULL},
#' so that the names are obtained from the dendrogram labels.
#' @return assignments for the consensus class
#' @author Derek Chiu
#' @export
consensusClass <- function(x, k, method = "average", names = NULL) {
  tree <- hclust(dist(x), method = method)
  cl <- as.factor(cutree(tree, k))
  if (!is.null(names))
    names(cl) <- names
  else
    names(cl) <- tree$labels
  return(cl)
}