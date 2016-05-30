#' Generate null distributions on empirical gene-gene correlations
#'
#' Using a principal component constructed from the sample space,
#' we simulate null distributions with univariate Normal distributions.
#'
#' @param data data matrix
#' @param nSim The number of simulated datasets to generate
#'
#' @return A list of length \code{nSim}. Each element is a pcNormal simulated
#' matrix.
#' @author Derek Chiu
#' @importFrom stats princomp rnorm
#' @export
#' @examples
#' A <- matrix(rnorm(300), nrow = 20)
#' pcDats <- pcNormal(A)
pcNormal <- function(data, nSim = 50) {
  pc <- princomp(data)
  Yns <- replicate(nSim, sapply(pc$sdev, function(x) rnorm(pc$n.obs, sd = x)),
                   simplify = FALSE)
  Qns <- lapply(Yns, function(x) x %*% t(pc$loadings))
  return(Qns)
}
