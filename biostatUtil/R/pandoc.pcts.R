pandoc.pcts <- function(char) {
  count <- as.integer(char[1])
  pcts <- as.numeric(char[-1]) * 100
  if (length(char) > 2)
    return(paste0(count, " (", pcts[1], "%, ", pcts[2], "%)"))
  else
    return(paste0(count, " (", pcts, "%)"))
}