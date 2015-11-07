#' Cluster validation indices
#' 
#' Compute internal evaluation indices for a set of algorithms.
#' 
#' @param dat data matrix with samples as rows, genes as columns
#' @param cl.mat matrix of cluster assignments. Each row is an assignment for
#' a different algorithm.
#' @param cons.mat A list of consensus matrices, one for each algorithm.
#' @param alg.names names for labeling the algorithms
#' @return A data frame of internal evaluation indices in each column, with
#' algorithms in each row
#' @author Derek Chiu
#' @export
indices <- function(dat, cl.mat, cons.mat, alg.names = NULL) {
  return(data.frame(
    Algorithms = alg.names,
    PAC = sapply(cons.mat, PAC, lower = 0.05, upper = 0.95),
    CHI = apply(cl.mat, 2, clusterSim::index.G1, x = dat)))
}