rowColPercent <- function(t, ...) {
  # generate a table with count, row %, column % given table t
  # i.e. the return table will have row = nrow(t)*3
  require(gdata)
  row.p <- rowPercent(t, ...)
  col.p <- colPercent(t, ...)
  result <- as.matrix(interleave(t, row.p, col.p))
  rownames(result)[1:nrow(result) %% 3 == 2] <- "Row %"
  rownames(result)[1:nrow(result) %% 3 == 0] <- "Col %"
  return(result)
}