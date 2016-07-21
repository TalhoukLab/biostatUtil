#' Rounding of Small Numbers
#' 
#' Rounding of a number smaller than specified precision doesn't coerce to 0.
#' 
#' This function is useful when we have small p-values and don't want to show the
#' scientific notation, or coercion to 0. Instead we show an upper bound. For example,
#' if a p-value is 2e-05 and we want to round to 3 digits, the function will return
#' "<0.001.
#'
#' @param ... a numeric vector
#' @param digits integer indicating number of decimal places to round to
#'
#' @return If precision of number is larger than desired rounding, the default
#' \code{round} is used. Otherwise, we provide an upper bound instead of coercion
#' to 0.
#' @author Derek Chiu
#' @export
#'
#' @examples
#' round_small(2e-04)
#' round_small(5e-04)
#' round_small(6e-04)
round_small <- function(..., digits = 3) {
  sapply(as.list(...), function(x) {
    if (is.na(x)) {
      return(NA)
    } else {
      if (x <= 5 * 10 ^ -(digits + 1)) {
        return(paste0("<", 1 * 10 ^ -digits))
      } else {
        return(round(x, digits = digits))
      }
    }
  })
}