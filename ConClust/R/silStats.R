#' Silhouette width statistics
#' 
#' Computes the fraction of negative silhouette widths and average of positive silhouette widths.
#'
#' @param dat data matrix
#' @param cl integer vector of cluster memberships
#'
#' @return A data frame with fN and aP
#' @author Derek Chiu
silStats <- function(dat, cl) {
  s <- cluster::silhouette(cl, dist(dat))
  fN <- sum(s[, "sil_width"] < 0) / nrow(s)
  aP <- mean(s[, "sil_width"][s[, "sil_width"] > 0])
  return(data.frame(fN, aP))
}
