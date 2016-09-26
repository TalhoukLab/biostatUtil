#' Hook Functions
#'
#' Custom hook functions for AGglomerative NESting and DIvisive ANAlysis
#' clustering algorithms and Mutual Information distance in
#' ConsensusClusterPlus.
#'
#' The hierarchical structure returned using \code{agnes} is equivalent to using
#' \code{\link{hclust}}.
#'
#' @param d distance matrix
#' @param k scalar indicating number of clusters to cut tree into
#' @return clustering assignment from \code{agnes} or \code{diana}; or for
#'   \code{mi_hook}, the mutual information distance matrix
#' @name hooks
#' @author Derek Chiu
#' @import cluster bioDist
#' @export
agnes_hook <- function(d, k) {
  tmp <- agnes(d, diss = TRUE)
  a <- cutree(tmp, k)
  return(a)
}

#' @rdname hooks
#' @export
diana_hook <- function(d, k) {
  tmp <- diana(d, diss = TRUE)
  a <- cutree(tmp, k)
  return(a)
}

#' @rdname hooks
#' @param x data matrix
#' @export
mi_hook <- function(x) {
  MIdist(x)
}
