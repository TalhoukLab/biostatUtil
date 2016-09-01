#' AGglomerative NESting
#'
#' A hook function for using agnes clustering as a custom
#' algorithm in ConsensusClusterPlus.
#'
#' The hierarchical structure returned using \code{agnes} is equivalent
#' to using \code{hclust}.
#'
#' @param this_dist distance matrix
#' @param k scalar indicating number of clusters to cut tree into
#' @return clustering assignment from agnes
#' @author Derek Chiu
#' @import cluster
#' @export
agnesHook = function(this_dist, k) {
  tmp <- agnes(this_dist, diss = TRUE)
  assignment <- cutree(tmp, k)
  return(assignment)
}
