#' Obtain consensus classes
#'
#' Performs hierarchical clustering on a consensus matrix to obtain
#' consensus class labels.
#'
#' @param x a consensus matrix
#' @param k desired number of clusters
#' @param method linkage type for hierarchical clustering
#' @return assignments for the consensus class
#' @author Derek Chiu
#' @export
consensusClass <- function(x, k, method = "average") {
  tree <- hclust(dist(x), method = method)
  cl <- cutree(tree, k)
  names(cl) <- tree$labels
  return(cl)
}