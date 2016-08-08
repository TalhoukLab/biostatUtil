#' Pairwise Correlations
#' 
#' Computes all pairwise correlations between the columns of a data frame
#' 
#' @param dataframe A data frame containing numeric variables of interest.
#' @return all pairwise absolute correlations, correlations, Pval, Adj P val by 
#'   decreasing order of absolute correlations.
#'   
#' @author Aline Talhouk, Derek Chiu
#' @export
#' 
#' @examples 
#' set.seed(123)
#' x <- data.frame(matrix(rnorm(25), nrow = 5))
#' pairwiseCor(x)
pairwiseCor <- function(dataframe) {
  if (!all(apply(dataframe, 2, is.numeric))) {
    stop("All columns of data matrix must be numeric")
  }
  . <- Cor <- AbsCor <- Pval <- AdjP <- NULL
  x <- dataframe
  pairs <- combn(colnames(dataframe), 2) %>% 
    set_rownames(paste0("Variable", 1:2))
  pairwiseCorDF <- data.frame(Cor = apply(pairs, 2, function(df) cor(x[, df]))[2, ]) %>% 
    mutate(AbsCor = abs(Cor),
           Pval = mapply(function(v1, v2) cor.test(v1, v2)$p.value,
                         x[pairs[1, ]], x[pairs[2, ]]),
           AdjP = p.adjust(Pval, "fdr")) %>% 
    select(AbsCor, Cor, Pval, AdjP) %>% 
    round(4) %>% 
    data.frame(t(pairs), ., stringsAsFactors = FALSE) %>% 
    arrange(desc(AbsCor))
  return(pairwiseCorDF)
}
