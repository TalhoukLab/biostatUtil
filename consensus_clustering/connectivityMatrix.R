connectivityMatrix <- function(clusters) {
  # Given a cluster membership vector, returns the connectivity matrix
  # 1 if both samples in same cluster, 0 otherwise
  require(dplyr)
  cm <- clusters %>%
    rep(., length(.)) %>%
    matrix(ncol = sqrt(length(.)))
  
  for (j in 1:ncol(cm)) {
    if (is.na(cm[j, j])) {
      cm[, j] <- 0
    } else {
      cm[, j] <- ifelse(cm[j, j] != cm[, j] | is.na(cm[, j]), 0, 1)
    }
  }
  
  rownames(cm) <- colnames(cm) <- names(clusters)
  return(cm)
}