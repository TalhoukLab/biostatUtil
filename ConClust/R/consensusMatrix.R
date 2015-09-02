#' Consensus matrix
#'
#' Returns the (weighted) consensus matrix given a data matrix
#'
#' Input matrix has rows as samples, columns as replicates. If a meta-consensus matrix
#' is desired, where consensus classes of different clustering algorithms are aggregated,
#' we can construct a weighted meta-consensus matrix using \code{weights}.
#'
#' @param dat data matrix
#' @param weights a vector of weights for each algorithm used in meta-consensus
#' clustering. Must have \code{length(weights)} equal to \code{ncol(dat)}.
#' @return a consensus matrix
#' @author Derek Chiu
#' @export
consensusMatrix <- function(dat, weights = NULL) {
  all.IM <- plyr::alply(dat, 2, indicatorMatrix)
  all.CM <- plyr::alply(dat, 2, connectivityMatrix)
  sum.IM <- Reduce('+', all.IM)
  if (!is.null(weights)) {
    weighted.CM <- mapply('*', all.CM, weights, SIMPLIFY = FALSE)
    sum.CM <- Reduce('+', weighted.CM) * length(weights)
  } else {
    sum.CM <- Reduce('+', all.CM)
  }
  cons.mat <- Reduce('/', list(sum.CM, sum.IM))
  return(cons.mat)
}
