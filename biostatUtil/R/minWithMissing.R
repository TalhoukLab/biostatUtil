#' Calculates minimum with specifying missing values
#' 
#' Returns the minimum of input vector, with the ability to specify
#' which values are missing
#' @param x input vector
#' @param missing.value missing values in \code{x}
#' @param return.missing.value character to return for missing values
#' @note \code{NAs} are ignored.
#' @author Samuel Leung
#' @export
#' @examples
#' z <- c(10:1)
#' minWithMissing(z, c(1, 3))
#' 
#' ## All missing
#' minWithMissing(z, c(1:10))
#' minWithMissing(z, c(1:10), return.missing.value = "all missing")
minWithMissing <- function(x, missing.value = -1, return.missing.value = -1) {
  x.missing <- x %in% missing.value
  
  if (sum(x.missing) == length(x) | sum(is.na(x)) == length(x))
    return(return.missing.value)

  return(min(as.numeric(x[!x.missing]), na.rm = TRUE))
}