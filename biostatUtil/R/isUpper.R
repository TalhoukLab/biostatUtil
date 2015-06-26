#' Is the first letter uppercase?
#' 
#' Returns a logical indicating whether the first letter
#' of a character string is uppercase.
#' 
#' If the input is an empty string, the function returns \code{TRUE}.
#' 
#' @param x character string
#' @return logical; if \code{TRUE}, the first letter of the input
#' string is uppercase.
#' @author Samuel Leung
#' @export
#' @examples 
#' isUpper("peanut butter")
#' isUpper("peanut Butter")
#' isUpper("Samuel butter")
#' isUpper("")
isUpper <- function(x) {
  arr <- strsplit(x, "")[[1]]
  if (length(arr) == 0) {
    return(TRUE)
  } else {
    return(arr[1] == toupper(arr[1]))
  }
}