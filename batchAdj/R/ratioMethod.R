#' Batch adjustment by taking the ratio of a reference sample
#' @param Y2 is the data run in the second batch, samples are rows and genes are columns
#' @param R1 is the reference data run in the first batch
#' @param R2 is the reference data run in the second batch
#' @return Y2new is the Y2 data adjusted to batch 1
#' @author Aline Talhouk
#' @export
ratioMethod <- function(Y2, R1, R2) {
  m <- apply(R1, 2, mean) - apply(R2, 2, mean)
  Y2new <- t(apply(Y2, 1, function(x){x + m}))
  return(Y2new)
}
