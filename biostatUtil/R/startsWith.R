#' Check if string starts in partciular way
#' @author Samuel Leung
#' @export
startsWith <- function(a, b) {
  if (length(grep(b, a)) == 0) {
    return(FALSE) # not even substring ... must be false
  } else {
    return(substr(a ,1, nchar(b)) == b)
  }
}
