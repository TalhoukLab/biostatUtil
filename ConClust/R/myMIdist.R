#' Mutual Information
#'
#' A hook function for using mutual information as a custom
#' distance metric in ConsensusClusterPlus.
#'
#' @param x data matrix
#' @return mutual information distance matrix
#' @author Derek Chiu
#' @import bioDist
#' @export
myMIdist = function(x) {
  MIdist(x)
}
