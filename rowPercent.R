rowPercent <- function(t, pretty.text = FALSE, digits = 4) {
  # Generate a table of row percentages given table t
  pcts <- t / apply(t, 1, sum)
  if (pretty.text) {
    pcts <- apply(pcts * 100, c(1, 2),
                  function(x) ifelse(!is.nan(x),
                                     paste0(format(x, digits = digits), "%"),
                                     "-"))
    return(pcts)
  } else {
    return(round(pcts, digits = digits))
  }
}