#' Escape string for regular expression
escapeForGrep <- function(x){
  sub("\\[","\\\\[",
      sub("\\]","\\\\]",
          sub("\\(","\\\\(",
              sub("\\)","\\\\)",x)
          )))
}