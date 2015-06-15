#' Row and column percentages
#' 
#' Calculate row and column percentages in a table.
#' 
#' A table with counts, row %, and column % are generated given table t.
#' The return table will have \code{nrow(t) * 3} rows.
#' 
#' @param t a matrix
#' @param ... additional arguments for \code{\link{rowPercent}}
#' or \code{\link{colPercent}}
#' @return A table with row and column percentages interleaved.
#' @author Aline Talhouk, Derek Chiu
#' @seealso \code{\link{rowPercent}} for row percentages only,
#' \code{\link{colPercent}} for column percentages only.
#' @export
#' @examples
#' mat <- matrix(c(5, 3, 8, 9, 2, 4), nrow = 2)
#' rowColPercent(mat)
#' rowColPercent(mat, pretty.text = TRUE)
rowColPercent <- function(t, ...) {
  row.p <- rowPercent(t, ...)
  col.p <- colPercent(t, ...)
  result <- as.matrix(gdata::interleave(t, row.p, col.p))
  rownames(result)[1:nrow(result) %% 3 == 2] <- "Row %"
  rownames(result)[1:nrow(result) %% 3 == 0] <- "Col %"
  return(result)
}