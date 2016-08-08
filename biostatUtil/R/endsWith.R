#' Check if a string ends with a partciular string
#' 
#' Returns a logical indicating whether the string in the first argument ends 
#' with the string in the second argument.
#' 
#' Although intended for strings, \code{endsWith} works with numeric vectors as
#' well.
#' 
#' @param a string to be checked against
#' @param b string ending to be checked
#'
#' @return logical; if TRUE, \code{a} is a string that ends with \code{b}.
#' @author Samuel Leung
#' @seealso \code{\link{startsWith}}
#' @export
#' @examples 
#' ## Strings
#' endsWith("pineapple", "apple")
#' endsWith("apple", "app")
#' 
#' ## Numerics
#' endsWith(123.4, 3.4)
#' endsWith(45, 4)
endsWith <- function(a, b) {
  if (length(grep(b, a)) == 0)
    return(FALSE)
  else
    return(substr(a, nchar(a) - nchar(b) + 1, nchar(a)) == b)
}