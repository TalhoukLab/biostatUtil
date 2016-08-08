#' Simple capitalization
#' 
#' Capitalize the first letter of every word in a character string.
#' 
#' To capitalize only the first word, use \code{first.only = TRUE}.
#' 
#' @param x character string
#' @param first.only logical; if \code{TRUE}, only the first word will be 
#'   capitalized
#' @return A character string with every word's first letter capitalized.
#' @author Samuel Leung
#' @references
#'   http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
#'   
#' @export
#' @examples 
#' simpleCap("clear cell")
#' simpleCap("high grade serous carcinoma")
#' simpleCap("ovarian cancer", first.only = TRUE)
simpleCap <- function(x, first.only = FALSE) {
  if (!first.only)
    s <- strsplit(x, " ")[[1]]
  else
    s <- x
  return(paste0(toupper(substring(s, 1, 1)), substring(s, 2),
                collapse = " "))
}