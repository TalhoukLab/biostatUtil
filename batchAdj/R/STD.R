#' Batch Standardization
#'
#' This functions adjust the gene expression level between to batches by removing the average expression of each gene across cases in each batch, and scaling by the standard deviation. The average log expression from batch 1 is added back
#' @param X1 matrix of gene expression from batch 1 (excluding References). Rows are samples cols are genes.
#' @param X2 matrix of gene expression from batch 2 (excluding References). Rows are samples cols are genes.
#' @note the column names of X1 and X2 have to match.
#' @return matrix of log normalized data in the same format but without reference genes.


STD <- function(X1, X2) {
  if(!all(colnames(X1) == colnames(X2))) {
    print("STOP THERE ARE ERRORS")
    }  
  X1mean <- apply(X1, 2, mean)
  Res <- standardization(rbind(X1, X2), factor(c(rep(1, nrow(X1)), rep(2, nrow(X2)))))
  Res <- t(apply(Res, 1, function(x) x + X1mean))
  return(Res)
}