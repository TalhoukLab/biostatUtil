#' Is the string parsable to a number?
#'
#' Checks whether a string can be parsed into a number. Blanks are ignored.
#'
#' @param x character string
#' @return logical. Returns \code{TRUE} if \code{x} is parsable to a numeric (i.e.
#' \code{x} is either already in numeric format or is a number formatted
#' as a character)
#' @author Samuel Leung
#' @export
#' @examples
#' isParsableToNumeric("example")
#' isParsableToNumeric("123")
#' isParsableToNumeric(123)
#' isParsableToNumeric(NA)
isParsableToNumeric <- function(x) {
  if (is.numeric(x))
    return(TRUE)
  x <- sapply(x, stringr::str_trim)
  x <- x[(!is.na(x)) & (x != "")]
  return(sum(suppressWarnings(!is.na(as.numeric(x)))) == length(x))
}
