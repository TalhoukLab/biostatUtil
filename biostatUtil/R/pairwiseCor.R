#' Pairwise Correlations
#' 
#' Computes all pairwise correlations between the columns of a data frame
#'
#' @param dataframe A data frame containing numeric variables of interest. 
#' @return all pairwise absolute correlations, correlations, Pval, Adj P val by decreasing order of absolute correlations.
#' @export
pairwiseCor <- function(dataframe) {
  
  # Check that the data provided is numeric 
  if (!all(apply(dataframe,2,is.numeric))) {
    stop("All columns of data matrix must be numeric")
  }
  pairs <- combn(colnames(dataframe), 2, simplify = FALSE)
  df <- data.frame(Vairable1 = rep(0, length(pairs)),
                   Variable2 = rep(0, length(pairs)), 
                   AbsCor = rep(0, length(pairs)),
                   Cor = rep(0, length(pairs)),
                   Pval = rep(0, length(pairs)),
                   AdjP = rep(0,length(pairs)))
  for (i in 1:length(pairs)) {
    df[i, 1] <- pairs[[i]][1]
    df[i, 2] <- pairs[[i]][2]
    df[i, 3] <- round(abs(cor(dataframe[, pairs[[i]][1]],
                              dataframe[, pairs[[i]][2]])), 4)
    df[i, 4] <- round(cor(dataframe[, pairs[[i]][1]],
                          dataframe[, pairs[[i]][2]]), 4)
    df[i, 5] <- round(cor.test(dataframe[, pairs[[i]][1]],
                               dataframe[, pairs[[i]][2]])$p.value, 4)
  }
  df[, 6] <- round(p.adjust(df[, 5], method = "fdr"), 4)
  pairwiseCorDF <- df
  pairwiseCorDF <- pairwiseCorDF[order(pairwiseCorDF$AbsCor,
                                       decreasing = TRUE), ]
  row.names(pairwiseCorDF) <- 1:length(pairs)
  pairwiseCorDF <<- pairwiseCorDF
  return(pairwiseCorDF)
}
