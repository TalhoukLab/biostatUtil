#' Confusion matrix between meta-consensus clusters and true classes
#' 
#' Combines the cluster assignments from multiple algorithms, then 
#' compares to the true class partitions.
#' 
#' @param cl.mat matrix or data frame, where each column is a cluster assignment
#' coming from a different algorithm, and each row is a different sample.
#' @param cl.true true class labels
#' @param k number of clusters
#' @param pred.lab label for the predicted clusters
#' @param ref.lab label for the reference classes
#' @param weight.algs Specify the weights remaining
#' @param weights Specify a weights to compute a weighted meta-consensus
#' clustering. By default set to \code{NULL} (unweighted).
#' @return A confusion matrix with the predicted meta-consensus cluster 
#' assignments compared to the reference true class labels.
#' @author Derek Chiu 
#' @importFrom magrittr set_names set_rownames
#' @export
consensusConfmat <- function(cl.mat, cl.true, k,
                             pred.lab = "Prediction", ref.lab = "Reference",
                             weight.algs = NULL, weights = NULL) {
  . <- NULL
  if (is.null(weight.algs) & is.null(weights)) {
    cm <- cl.mat %>% 
      consensusMatrix
  } else {
    cm <- cl.mat %>% 
      extract(, match(weight.algs, colnames(.))) %>% 
      consensusMatrix(weights)
  }
  ccm <- cm %>% 
    consensusClass(k) %>% 
    as.factor() %>%
    set_names(cl.true) %>%
    table(., names(.), dnn = c(eval(pred.lab), eval(ref.lab))) %>%
    extract(names(sort(apply(., 1, which.max))), ) %>%
    set_rownames(colnames(.)) %>%
    as.table() %>%
    caret::confusionMatrix(.)
  return(ccm)
}