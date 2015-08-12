#' Batch Effect Removal
#'
#' This function adjusts the gene expression level between to batches using ber with bagging (1000 sims).
#' @param X1 matrix of gene expression from batch 1 (excluding References). Rows are samples cols are genes.
#' @param X2 matrix of gene expression from batch 2 (excluding References). Rows are samples cols are genes.
#' @note the column names of X1 and X2 have to match.
#' @return matrix of log normalized data in the same format but without reference genes.
#' @export
BER <- function(X1, X2){
  if (!all(colnames(X1) == colnames(X2))) {
    print("STOP THERE ARE ERRORS")
  }
  Res <- ber::ber_bg(rbind(X1, X2),
                     factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))),
                     nSim = 1000)
  return(Res)
}
