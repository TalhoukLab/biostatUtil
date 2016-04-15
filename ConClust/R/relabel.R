#' Relabel clustering categories to match to a standard
#'
#' Relabel clustering categories to match to a standard by minimizing the minimum in Frobenius norm between the two labels.
#'
#' @param clust vector of cluster assignments
#' @param cl.ref reference labels to match to
#' @param pred.lab label for the predicted clusters
#' @param ref.lab label for the reference classes
#' @return A vector of relabeled cluster assignments
#' @author Aline Talhouk
#' @importFrom magrittr set_names set_rownames use_series
#' @export
classRelabel <- function(clust, cl.ref) {
  perm <- table(clust, cl.ref) %>%
    minFnorm() %>%
    use_series(perm) 
  res <- factor(clust,levels = perm, labels=levels(factor(cl.ref)))
    return(res)
}
