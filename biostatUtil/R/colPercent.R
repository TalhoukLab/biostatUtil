#' Column percentages
#' 
#' Calculate column percentages in a table.
#' 
#' Generates a table of row percentages given table \code{t}. Using
#' \code{pretty.text = TRUE} will add the \% sign to the percentages.
#' 
#' @param t a matrix
#' @param pretty.text logical. If \code{TRUE}, will format the table into nice
#' display
#' @param keep logical. If \code{TRUE}, the original table counts will be kept
#' along with column percentages.
#' @param digits number of digits to round to
#' @return A table with column-wise percentages added. For every column,
#' the percentages sum to 1.
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples
#' A <- matrix(c(2, 3, 5, 10), nrow = 2)
#' colPercent(A)
#' colPercent(A, pretty.text = TRUE)
#' colPercent(A, pretty.text = TRUE, keep = TRUE)
colPercent <- function(t, pretty.text = FALSE, keep = TRUE, digits = 4) {
  pcts <- apply(t, 2, function(x) x / sum(x))
  if (pretty.text) {
    pcts <- apply(pcts * 100, c(1, 2),
                  function(x) ifelse(!is.nan(x),
                                     paste0(format(x, digits = digits), "%"),
                                     "-"))
    if (keep)
      return(rbind(t, pcts))
    else
      return(pcts)  
  } else {
    if (keep)
      return(rbind(t, round(pcts, digits = digits)))
    else
      return(round(pcts, digits = digits))
  }
}