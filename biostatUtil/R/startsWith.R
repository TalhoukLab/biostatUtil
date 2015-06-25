#' Check if a string starts with a partciular string
#' 
#' Returns a logical indicating whether the string in the first argument starts
#' with the string in the second argument.
#' 
#' Although intended for strings, \code{startsWith} works with numeric vectors
#' as well.
#' 
#' @param a string to be checked against
#' @param b string ending to be checked
#' @return logical; if TRUE, \code{a} is a string that starts with \code{b}.
#' @author Samuel Leung
#' @seealso \code{\link{endsWith}}
#' @export
#' @examples 
#' ## Strings
#' startsWith("pineapple", "pine")
#' startsWith("apple", "app")
#' startsWith("apple", "ppl")
#' 
#' ## Numerics
#' startsWith(123.4, 123)
#' startsWith(45, 5)
startsWith <- function(a, b) {
  if (length(grep(b, a)) == 0) {
    return(FALSE)  # not even substring ... must be false
  } else {
    return(substr(a, 1, nchar(b)) == b)
  }
}