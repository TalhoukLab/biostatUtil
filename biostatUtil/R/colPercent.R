#' Column percentages
#' 
#' Generates a table of column percentages given a table.
#' 
#' Details to be filled.
#' @param t a matrix
#' @param pretty.text logical indicating whether to format the table
#' @param keep logical indicating whether to keep the raw table values
#' @param digits number of digits to round to
#' @return A table with column percentages added.
#' @author Aline Talhouk, Derek Chiu
#' @export
colPercent <- function(t, pretty.text = FALSE, keep = FALSE, digits = 4) {
  pcts <- apply(t, 2, function(x) x / sum(x))
  if (pretty.text) {
    pcts <- apply(pcts * 100, c(1, 2),
                  function(x) ifelse(!is.nan(x),
                                     paste0(format(x, digits = digits), "%"),
                                     "-"))
    if (keep) {
      return(rbind(t, pcts))
    } else {
      return(pcts)  
    }
  } else {
    if (keep) {
      return(rbind(t, round(pcts, digits = digits)))
    } else {
      return(round(pcts, digits = digits))
    }
  }
}