#' Standardize a matrix
standardize <- function(x){
  annAll <- dimnames(x)
  x <- scale(x) # scale is generic function whose DEFAULT method 
  # CENTERS and SCALES the columns of a numeric matrix.
  # - centering is done by subtracting the column means 
  #   (omitting NAs) of x from their corresponding columns
  # - scaling is done by dividing the (centered) 
  #   columns of x by their standard deviations
  dimnames(x) <- annAll # reset row/column names back to original
  return(x)
}