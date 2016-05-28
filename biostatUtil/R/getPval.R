#' Get the p-value
#' @param x a vector or matrix
#' @return the Chi-squared p-value
#' @references Robert Gentleman (http://tolstoy.newcastle.edu.au/R/help/01c/2809.html)
#' @importFrom stats pchisq
#' @export
getPval <- function(x) { 
  if (is.matrix(x$obs)) 
    etmp <- apply(x$exp, 1, sum) 
  else 
    etmp <- x$exp 
  df <- (sum(1 * (etmp > 0))) - 1 
  return(1 - pchisq(x$chisq, df))
}