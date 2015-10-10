#' Batch Mean Centering
#'
#' This function adjusts the gene expression level between two batches by removing the average expression of
#' each gene across cases in each batch, and adding back the average log expression from batch 1.
#' @param X1 matrix of gene expression from batch 1 (excluding References). Rows are samples, columns are genes.
#' @param X2 matrix of gene expression from batch 2 (excluding References). Rows are samples, columns are genes.
#' @return a combined matrix of log normalized data in the same format but without reference genes.
#' @note The column names of X1 and X2 have to match.
#' @author Aline Talhouk
#' @seealso See \code{\link{BER}}, \code{\link{COMBAT}}, and \code{\link{STD}} for
#' other methods of batch effect removal.
#' @export
#' @examples
#' set.seed(12)
#' nc <- 10
#' A <- matrix(rnorm(120), ncol = nc)
#' B <- matrix(rnorm(80), ncol = nc)
#' colnames(A) <- colnames(B) <- letters[1:nc]
#' BMC(A, B)
BMC <- function(X1, X2) {
  assertthat::assert_that(check_ncol(X1, X2))
  assertthat::assert_that(all(colnames(X1) == colnames(X2)))
  X1mean <- apply(X1, 2, mean)
  Res <- ber::mean_centering(rbind(X1, X2),
                             factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))))
  Res <- t(apply(Res, 1, function(x) x + X1mean))
  return(Res)
}
