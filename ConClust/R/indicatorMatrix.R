#' Indicator matrix
#'
#' Given a vector of cluster memberships, the indicator matrix is computed.
#'
#' The indicator matrix has a 1 if both samples were selected to be used in
#' a subsample of a consensus clustering algorithm, and 0 otherwise.
#' @param clusters vector of cluster memberships
#' @return A \code{length(clusters)} by \code{length(clusters)} symmetric
#' indicator matrix.
#' @author Derek Chiu
#' @export
indicatorMatrix <- function(clusters) {
  . <- NULL
  im <- clusters %>%
    rep(., length(.)) %>%
    matrix(ncol = sqrt(length(.)))

  for (j in 1:ncol(im)) {
    if (is.na(im[j, j])) {
      im[, j] <- 0
    } else {
      im[, j] <- ifelse(is.na(im[, j]), 0, 1)
    }
  }

  rownames(im) <- colnames(im) <- names(clusters)
  return(im)
}
