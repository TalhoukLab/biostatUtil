#' Batch Effect Removal
#'
#' Adjusts the gene expression level between two batches using batch effect removal.
#'
#' \code{BER} uses bagging to remove batch effects. The default number of bootstrap samples is 1000. Batch Mean
#' Centering is performed using \code{BMC}, which adjusts the gene expression level between two batches by
#' removing the average expression of each gene across cases in each batch, and adding back the average log
#' expression from batch 1. Parametric \code{COMBAT} is another batch effect removal method. Finally, batch
#' standardization using \code{STD} removes the average expression of each gene across cases in each batch,
#' and then scales by the standard deviation. The average log expression from batch 1 is added back.
#'
#' @param X1 matrix of gene expression from batch 1 (excluding References). Rows are samples, columns are genes.
#' @param X2 matrix of gene expression from batch 2 (excluding References). Rows are samples, columns are genes.
#' @param nSim number of bootstrap samples
#' @return A matrix of log normalized data in the same format but without reference genes.
#' @note The column names of \code{X1} and \code{X2} have to match.
#' @author Aline Talhouk
#' @export
#' @examples
#' set.seed(12)
#' nc <- 10
#' A <- matrix(rnorm(120), ncol = nc)
#' B <- matrix(rnorm(80), ncol = nc)
#' colnames(A) <- colnames(B) <- letters[1:nc]
#' BER(A, B)
#' BMC(A, B)
#' COMBAT(A, B)
#' STD(A, B)
BER <- function(X1, X2, nSim = 1000) {
  assertthat::assert_that(check_ncol(X1, X2))
  assertthat::assert_that(all(colnames(X1) == colnames(X2)))
  Res <- ber::ber_bg(rbind(X1, X2),
                     factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))),
                     nSim = nSim)
  return(Res)
}

#' @rdname BER
#' @export
BMC <- function(X1, X2) {
  assertthat::assert_that(check_ncol(X1, X2))
  assertthat::assert_that(all(colnames(X1) == colnames(X2)))
  X1mean <- apply(X1, 2, mean)
  Res <- ber::mean_centering(rbind(X1, X2),
                             factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))))
  Res <- t(apply(Res, 1, function(x) x + X1mean))
  return(Res)
}

#' @rdname BER
#' @export
COMBAT <- function(X1, X2) {
  assertthat::assert_that(check_ncol(X1, X2))
  assertthat::assert_that(all(colnames(X1) == colnames(X2)))
  Res <- mycombat_p(rbind(X1, X2),
                    factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))))
  return(Res)
}

#' @rdname BER
#' @export
STD <- function(X1, X2) {
  assertthat::assert_that(check_ncol(X1, X2))
  assertthat::assert_that(all(colnames(X1) == colnames(X2)))
  X1mean <- apply(X1, 2, mean)
  Res <- ber::standardization(rbind(X1, X2),
                              factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))))
  Res <- t(apply(Res, 1, function(x) x + X1mean))
  return(Res)
}
