#' Escape string for regular expression
#' @export
escapeForGrep <- function(x){
  sub("\\[", "\\\\[",
      sub("\\]", "\\\\]",
          sub("\\(", "\\\\(",
              sub("\\)", "\\\\)", x)
          )
      )
  )
}