#' Column percentages
#' 
#' Calculate column percentages in a table.
#' 
#' Generates a table of row percentages given table \code{t}. Using 
#' \code{pretty.text = TRUE} will add the \% sign to the percentages.
#' 
#' @param t a matrix
#' @param pretty.text logical. If \code{TRUE}, will format the table into nice 
#'   display
#' @param keep logical. If \code{TRUE}, the original table counts will be kept 
#'   along with column percentages.
#' @param digits number of digits to round to
#' @return A table with column-wise percentages added. For every column, the
#'   percentages sum to 1.
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples
#' A <- matrix(c(2, 3, 5, 10), nrow = 2,
#' dimnames = list(c("Row1", "Row2"), c("Col1", "Col2")))
#' colPercent(A)
#' colPercent(A, keep = FALSE)
#' colPercent(A, pretty.text = TRUE)
#' colPercent(A, pretty.text = TRUE, keep = FALSE)
colPercent <- function(t, pretty.text = FALSE, keep = TRUE, digits = 4) {
  pcts <- round(apply(t, 2, function(x) x / sum(x)), digits = digits)
  if (pretty.text)
    pcts <- apply(pcts * 100, c(1, 2),
                  function(x) ifelse(!is.nan(x), paste0(format(x), "%"), "-"))
  if (keep) {
    pcts <- rbind(t, pcts)
    rownames(pcts) <- paste0(rownames(pcts), rep(c("", " %"), each = 2))
    return(pcts)
  } else {
    return(pcts)
  }
}