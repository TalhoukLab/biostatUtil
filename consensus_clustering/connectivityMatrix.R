
connectivityMatrix <- function(clusters) {
  # Given a cluster membership vector, returns the connectivity matrix
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

# get CM for R1
connectivityMatrix(results.nmf[, 1, 1])

# Store CMs for 1000 reps (runs in < 90 secs)
library(plyr)
all.CM <- alply(.data = results.nmf[, , 1], .margins = 2, .fun = connectivityMatrix)
