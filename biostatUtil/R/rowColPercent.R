#' Row and column percentages
#' 
#' Calculate row and column percentages in a table.
#' 
#' Details to be filled.
#' 
#' @param t a matrix
#' @param ... additional arguments for \code{\link{rowPercent}}
#' or \code{\link{colPercent}}
#' @return A table with row and column percentages interleaved.
#' @author Aline Talhouk, Derek Chiu
#' @seealso \code{\link{rowPercent}} for row percentages only,
#' \code{\link{colPercent}} for column percentages only.
rowColPercent <- function(t, ...) {
  # generate a table with count, row %, column % given table t
  # i.e. the return table will have row = nrow(t)*3
  row.p <- rowPercent(t, ...)
  col.p <- colPercent(t, ...)
  result <- as.matrix(gdata::interleave(t, row.p, col.p))
  rownames(result)[1:nrow(result) %% 3 == 2] <- "Row %"
  rownames(result)[1:nrow(result) %% 3 == 0] <- "Col %"
  return(result)
}