#' Row percentages
#' 
#' Calculate row percentages.
#' 
#' Generates a table of row percentages given table \code{t}. Using
#' \code{pretty.text = TRUE} will add the \% sign to the percentages.
#' 
#' @param t a matrix
#' @param pretty.text logical. If \code{TRUE}, will format the table into nice
#' display
#' @param keep logical. If \code{TRUE}, the original table counts will be kept
#' along with row percentages.
#' @param digits number of digits to round to
#' @return A table with row-wise percentages added. For every row,
#' the percentages sum to 1.
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples
#' A <- matrix(c(2, 3, 5, 10), nrow = 2)
#' rowPercent(A)
#' rowPercent(A, pretty.text = TRUE)
#' rowPercent(A, pretty.text = TRUE, keep = TRUE)
rowPercent <- function(t, pretty.text = FALSE, keep = TRUE, digits = 4) {
  if(is.matrix(t)) {
    pcts <- t / apply(t, 1, sum)
    if (pretty.text) {
      pcts <- apply(pcts * 100, c(1, 2),
                    function(x) ifelse(!is.nan(x),
                                       paste0(format(x, digits = digits), "%"),
                                       "-"))
      if (keep) {
        return(rbind(t, row.percent = pcts))
      } else {
        return(pcts)  
      }
    } else {
      if (keep) {
        return(rbind(t, row.percent = round(pcts, digits = digits)))
      } else {
        return(round(pcts, digits = digits))
      }
    }
  } else {
    pcts <- t / sum(t)
    if (pretty.text) {
      pcts <- sapply(pcts * 100, function(x) {
          paste0(format(x, digits = digits), "%")}, USE.NAMES = FALSE)
      if (keep) {
        return(rbind(t, row.percent = pcts))
      } else {
        return(pcts)  
      }
    } else {
      if (keep) {
        return(rbind(t, row.percent = round(pcts, digits = digits)))
      } else {
        return(round(pcts, digits = digits))
      }
    }
  }
}