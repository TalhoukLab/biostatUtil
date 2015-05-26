indicatorMatrix <- function(clusters) {
  # Given a cluster membership vector, returns the indicator matrix
  # 1 if both samples selected in subsample, 0 otherwise
  require(dplyr)
  im <- clusters %>%
    rep(., length(.)) %>%
    matrix(ncol = sqrt(length(.)))
  
  for (j in 1:ncol(im)) {
    if (is.na(im[j, j])) {
      im[, j] <- 0
    } else {
      im[, j] <- ifelse(is.na(im[, j]), 0, 1)
    }
  }
  
  rownames(im) <- colnames(im) <- names(clusters)
  return(im)
}