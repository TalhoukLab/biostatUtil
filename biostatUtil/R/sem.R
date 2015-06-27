#' Standard error of the mean
#' @author Samuel Leung
#' @references http://en.wikipedia.org/wiki/Standard_error
#' @export
sem <- function(x, missing.value = NA, return.missing.value = NA) {
  x <- x[!is.na(x)]
  if (!is.na(missing.value)) {
    x <- x[!x %in% missing.value]
  }
  return(ifelse(length(x) == 0,
                return.missing.value,
                sqrt(var(x) / length(x))))
}