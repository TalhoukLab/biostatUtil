#' Standard error of the mean
#' 
#' @param x input vector
#' @param missing.value values that are missing
#' @param return.missing.value the value to return where there are missing values
#' @return The standard error of the mean of \code{x}
#' @author Samuel Leung
#' @references http://en.wikipedia.org/wiki/Standard_error
#' @importFrom stats var
#' @export
sem <- function(x, missing.value = NA, return.missing.value = NA) {
  x <- x[!is.na(x)]
  if (!is.na(missing.value))
    x <- x[!x %in% missing.value]
  return(ifelse(length(x) == 0, return.missing.value,
                sqrt(var(x) / length(x))))
}