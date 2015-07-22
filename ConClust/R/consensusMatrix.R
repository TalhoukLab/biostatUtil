#' Consensus matrix
#'
#' Returns the consensus matrix given a data matrix
#'
#' Input matrix has rows as samples, columns as replicates.
#' @param dat data matrix
#' @return a consensus matrix
#' @author Derek Chiu
#' @export
consensusMatrix <- function(dat) {
  all.CM <- plyr::alply(dat, 2, connectivityMatrix)
  all.IM <- plyr::alply(dat, 2, indicatorMatrix)

  sum.CM <- Reduce('+', all.CM)
  sum.IM <- Reduce('+', all.IM)

  cons.mat <- Reduce('/', list(sum.CM, sum.IM))
  return(cons.mat)
}
