#' Connectivity matrix
#'
#' Given a vector of cluster memberships, the connectivity matrix is computed.
#'
#' The connectivity matrix has a 1 if both samples are in the same cluster,
#' and 0 otherwise.
#' @param clusters vector of cluster memberships
#' @return A \code{length(clusters)} by \code{length(clusters)} symmetric
#' connectivity matrix.
#' @author Derek Chiu
#' @export
connectivityMatrix <- function(clusters) {
  . <- NULL
  cm <- clusters %>%
    rep(., length(.)) %>%
    matrix(ncol = sqrt(length(.)))

  for (j in 1:ncol(cm)) {
    if (is.na(cm[j, j])) {
      cm[, j] <- 0
    } else {
      cm[, j] <- ifelse(cm[j, j] != cm[, j] | is.na(cm[, j]), 0, 1)
    }
  }

  rownames(cm) <- colnames(cm) <- names(clusters)
  return(cm)
}
