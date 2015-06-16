#' Get the p-value
#' @export
getPval <- function(x) 
{ 
  if( is.matrix(x$obs)) 
    etmp <- apply(x$exp, 1, sum) 
  else 
    etmp <- x$exp 
  df<- (sum(1 * (etmp > 0))) - 1 
  pv <- 1 - pchisq(x$chisq, df) 
  pv 
} 