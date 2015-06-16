#' Check if string ends in partciular way
#' @export
endsWith <- function(a, b) {
  if (length(grep(b, a))==0) {
    return(FALSE) # not even substring ... must be false
  } else {
    return(substr(a, nchar(a) - nchar(b) + 1, nchar(a)) == b)
  }
}