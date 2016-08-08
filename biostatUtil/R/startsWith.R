#' Check if a string starts or ends with another string
#' 
#' Returns a logical indicating whether the string in the first argument starts 
#' or ends with the string in the second argument.
#' 
#' Although intended for strings, the function works with numeric vectors as
#' well.
#' 
#' @param a string to be checked against
#' @param b prefix/suffix to check
#' @return logical; if TRUE, \code{a} is a string that starts or ends with
#'   \code{b}.
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples 
#' # Strings
#' startsWith("pineapple", "pine")
#' startsWith("apple", "app")
#' startsWith("apple", "ppl")
#' endsWith("pineapple", "apple")
#' endsWith("apple", "app")
#' 
#' # Numerics
#' startsWith(123.4, 123)
#' startsWith(45, 5)
#' endsWith(123.4, 3.4)
#' endsWith(45, 4)
startsWith <- function(a, b) {
  if (!grepl(b, a))
    return(FALSE)
  else
    return(substr(a, 1, nchar(b)) == b)
}

#' @inheritParams startsWith
#' @rdname startsWith
#' @export
endsWith <- function(a, b) {
  if (!grepl(b, a))
    return(FALSE)
  else
    return(substr(a, nchar(a) - nchar(b) + 1, nchar(a)) == b)
}