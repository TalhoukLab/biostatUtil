#' Normalized Mutual Information
#' 
#' Computes the NMI given a clustering assignment and true class labels.
#' 
#' The function is adapted from the \code{mutinformation} function in the \code{infotheo}
#' package.
#' @param X clustering assignment
#' @param Y true class
#' @param method method of computing the entropy. Can be any one of "emp", "mm", "shrink", or "sg".
#' @return returns the normalized mutual information.
#' @author Derek Chiu
#' @references Strehl A, Ghosh J. Cluster ensembles: a knowledge reuse framework for combining multiple partitions.
#' J. Mach. Learn. Res. 2002;3:583-617.
#' @export
NMI <- function(X, Y, method = "emp") {
  U <- data.frame(Y, X)
  Hyx <- infotheo::entropy(U, method)
  Hx <- infotheo::entropy(X, method)
  Hy <- infotheo::entropy(Y, method)
  I <- ifelse(Hx + Hy - Hyx < 0, 0, Hx + Hy - Hyx)
  NMI <- I / sqrt(Hx * Hy)
  return(NMI)
}