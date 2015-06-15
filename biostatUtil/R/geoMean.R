#' Geometric mean
geoMean <- function(x) {
  x <- x[!is.na(x)]
  result <- NA
  if (length(x) > 0) {
    result <- prod(x) ^ (1 / length(x))
  }
  return(result)
}