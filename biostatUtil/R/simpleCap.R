#' Simple capitalization
#' @export
simpleCap <- function(x, first.word.only = FALSE) {
  if (!first.word.only) {
    s <- strsplit(x, " ")[[1]]
  } else {
    s <- x
  }
  return(paste(toupper(substring(s, 1, 1)), substring(s, 2),
               sep = "", collapse =" "))
}