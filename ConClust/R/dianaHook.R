#' DIvisive ANAlysis Clustering
#'
#' A hook function for using diana clustering as a custom
#' algorithm in ConsensusClusterPlus.
#'
#' @param this_dist distance matrix
#' @param k scalar indicating number of clusters to cut tree into
#' @return clustering assignment from diana
#' @author Derek Chiu
#' @import cluster
#' @export
dianaHook = function(this_dist, k){
  tmp <- diana(this_dist, diss = TRUE)
  assignment <- cutree(tmp, k)
  return(assignment)
}
