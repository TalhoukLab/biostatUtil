#' Batch Standardization
#'
#' This function adjusts the gene expression level between two batches by removing the average expression of
#' each gene across cases in each batch, and scaling by the standard deviation. The average log expression from batch 1 is added back.
#' @param X1 matrix of gene expression from batch 1 (excluding References). Rows are samples, columns are genes.
#' @param X2 matrix of gene expression from batch 2 (excluding References). Rows are samples, columns are genes.
#' @return matrix of log normalized data in the same format but without reference genes.
#' @note The column names of X1 and X2 have to match.
#' @author Aline Talhouk
#' @seealso \code{\link{BER}}, \code{\link{BMC}}, \code{\link{COMBAT}}
#' @export
#' @examples
#' set.seed(12)
#' nc <- 10
#' A <- matrix(rnorm(120), ncol = nc)
#' B <- matrix(rnorm(80), ncol = nc)
#' colnames(A) <- colnames(B) <- letters[1:nc]
#' STD(A, B)
STD <- function(X1, X2) {
  assertthat::assert_that(check_ncol(X1, X2))
  assertthat::assert_that(all(colnames(X1) == colnames(X2)))
  X1mean <- apply(X1, 2, mean)
  Res <- ber::standardization(rbind(X1, X2),
                              factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))))
  Res <- t(apply(Res, 1, function(x) x + X1mean))
  return(Res)
}
