#' Find occurences of string within another string
#' 
#' Returns a vector of the indices where a string occurs in another string
#' 
#' If \code{b} is longer than \code{a}, \code{indexOf} returns \code{NA},
#' since it is not possible for a longer string to occur in a shorter
#' string.
#' 
#' @param a string to be checked against
#' @param b string to check
#' @param ignore.case logical; if \code{TRUE}, case is ignored when
#' performing the check
#' @return Indices where \code{b} occurs in \code{a}. Returns \code{NA}
#' if there are no occurences.
#' @author Samuel Leung
#' @export
#' @examples 
#' indexOf("derek", "e")
#' indexOf("Animals", "a")
#' indexOf("Animals", "A")
#' indexOf("Animals", "a", ignore.case = TRUE)
indexOf <- function(a, b, ignore.case = FALSE) {
  if (ignore.case) {
    a <- toupper(a)
    b <- toupper(b)
  }
  b.len <- nchar(b)
  a.len <- nchar(a)
  if (b.len > a.len) {
    return(NA)
  }
  a.arr <- strsplit(a, "")[[1]]
  indexes <- c()
  i <- 1
  while (i <= (a.len - b.len + 1)) {
    if (paste(a.arr[i:(i + b.len - 1)], collapse = "") == b) {
      indexes <- c(indexes, i)
      i <- i + b.len - 1
    }
    i <- i + 1
  }
  if (length(indexes) > 0) {
    return(indexes)
  } else {
    return(NA)
  }
}