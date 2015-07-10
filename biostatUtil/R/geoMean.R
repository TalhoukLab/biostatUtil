#' Geometric mean
#' 
#' Calculates the geometric mean
#' 
#' @param x a vector. Matrices are allowed.
#' @param na.rm logical; should NA values be removed?
#' @return If \code{x} has any \code{NA} values then \code{geoMean}
#' will return \code{NA} just like \code{mean} unless \code{na.rm = TRUE}
#' is specified. Otherwise, the geometric mean of \code{x} is returned.
#' If the input only has \code{NA} values then \code{NA} is returned as well.
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples
#' set.seed(1)
#' x <- rexp(100, rate = 0.5)
#' y <- rexp(100, rate = 0.3)
#' y[3] <- NA
#' geoMean(x)
#' geoMean(y)
#' geoMean(y, na.rm = TRUE)
geoMean <- function(x, na.rm = FALSE) {
  if (na.rm) {
    x <- x[!is.na(x)]
    if (length(x) > 0)
      result <- prod(x) ^ (1 / length(x))
    else
      result <- NA
  } else {
    result <- prod(x) ^ (1 / length(x))
  }
  return(result)
}