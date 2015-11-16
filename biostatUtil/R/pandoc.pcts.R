pandoc.pcts <- function(char) {
  count <- as.integer(char[1])
  pcts <- as.numeric(char[2:3]) * 100
  return(paste0(count, " (", pcts[1], "%, ", pcts[2], "%)"))
}