colPercent <- function(t, pretty.text = FALSE, keep = FALSE, digits = 4) {
  # Generate a table of column percentages given the table t
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