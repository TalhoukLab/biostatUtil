#' Escape string for regular expression
#' 
#' Escape [, ], (, and ) for use in \code{grep}.
#' @param x a character vector
#' @return A character vector with opening and closing square brackets and parentheses
#' escaped for use in \code{grep}.
#' @author Samuel Leung
#' @export
#' @examples
#' escapeForGrep("[index]")
#' escapeForGrep("(parentheses)")
escapeForGrep <- function(x){
  sub("\\[", "\\\\[",
      sub("\\]", "\\\\]",
          sub("\\(", "\\\\(",
              sub("\\)", "\\\\)", x)
          )
      )
  )
}