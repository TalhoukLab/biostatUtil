#' Batch Effect Removal with Bagging
#'
#' This function adjusts the gene expression level between two batches using BER with bagging.
#' @param X1 matrix of gene expression from batch 1 (excluding References). Rows are samples, columns are genes.
#' @param X2 matrix of gene expression from batch 2 (excluding References). Rows are samples, columns are genes.
#' @param nSim number of bootstrap samples (defaults to 1000).
#' @return matrix of log normalized data in the same format but without reference genes.
#' @note The column names of X1 and X2 have to match.
#' @author Aline Talhouk
#' @seealso See \code{\link{BMC}}, \code{\link{COMBAT}}, and \code{\link{STD}} for
#' other methods of batch effect removal.
#' @export
#' @examples
#' set.seed(12)
#' nc <- 10
#' A <- matrix(rnorm(120), ncol = nc)
#' B <- matrix(rnorm(80), ncol = nc)
#' colnames(A) <- colnames(B) <- letters[1:nc]
#' BER(A, B)
BER <- function(X1, X2, nSim = 1000) {
  assertthat::assert_that(check_ncol(X1, X2))
  assertthat::assert_that(all(colnames(X1) == colnames(X2)))
  Res <- ber::ber_bg(rbind(X1, X2),
                     factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))),
                     nSim = nSim)
  return(Res)
}
