#' Confusion matrix between consensus clusters and true classes
#'
#' Combines the cluster assignments from multiple algorithms, then
#' compares to the true class partitions.
#'
#' @param clust vector of cluster assignments
#' @param cl.true true class labels
#' @param k number of clusters
#' @param pred.lab label for the predicted clusters
#' @param ref.lab label for the reference classes
#' @return A confusion matrix with the predicted cluster
#' assignments compared to the reference true class labels.
#' @author Derek Chiu
#' @importFrom magrittr set_names set_rownames
#' @export
consensusConfmat <- function(clust, cl.true, k,
                             pred.lab = "Prediction", ref.lab = "Reference") {
  . <- NULL
  table(clust, cl.true, dnn = c(eval(pred.lab), eval(ref.lab))) %>%
    minFnorm() %>%
    use_series(pmat) %>% 
    set_rownames(colnames(.)) %>%
    as.table() %>%
    return()
}
