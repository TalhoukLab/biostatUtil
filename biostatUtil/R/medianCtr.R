#' Median center rows
medianCtr <- function(x) {
  annAll <- dimnames(x)
  medians <- apply(x, 1, median, na.rm = T)  # calculate median for each row
  
  # center each row of matrix so that all rows (genes) have a median of approx. 0
  x <- t(scale(t(x), center = medians, scale = F)) # scale is generic function whose default method 
  # centers and/or scales the COLUMNS of a numeric matrix. 
  dimnames(x) <- annAll # reset the row/column names back to original ones
  return(x)
}