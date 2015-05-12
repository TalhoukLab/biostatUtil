rowPercentSingle <- function(t, pretty.text = FALSE, digits = 4) {
  # Generate a table of row percentages given a table with a single row
  pcts <- t / sum(t)
  if (pretty.text) {
    pcts <- sapply(pcts * 100,
                   function(x) {
                     paste(format(x, digits = digits), "%", sep = "")
                   },
                   USE.NAMES = FALSE)
    return(rbind(t, pcts))
  } else {
    return(round(rbind(t, pcts), digits = digits))
  }
}