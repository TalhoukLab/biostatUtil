#' Is the first letter uppercase?
#' 
#' Returns a logical indicating whether the first letter of a character string
#' is uppercase.
#' 
#' If the input is an empty string, the function returns \code{TRUE}.
#' 
#' @param x character string
#' @return logical; if \code{TRUE}, the first letter of the input string is
#'   uppercase.
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples 
#' isFirstLetterUpperCase("peanut butter")
#' isFirstLetterUpperCase("peanut Butter")
#' isFirstLetterUpperCase("Samuel butter")
#' isFirstLetterUpperCase("")
isFirstLetterUpperCase <- function(x) {
  return(sub("(.).*", "\\1", x) %in% c(LETTERS, ""))
}