#' Compute the Dispersion Separability Criterion (DSC) which indicates how similar two data sets are by considering the between data scatter matrix and the withing data scatter matrix
#' @param data is a combined data set
#' @param Y a vector indicating the different batches
#' @param permut, the number of permutations to compute a pvalue for DSC using a permutation test.
#' @author Aline Talhouk
#' @export

DSC <- function(data, Y, permut=10) {
  scatmat <- scattermat(data, Y)
  DSC.observed <- sqrt(sum(diag(scatmat$B))) / sqrt(sum(diag(scatmat$W)))

  number_of_permutations <- permut

  DSC.random <- NULL
  for (i in 1 : number_of_permutations) {
    levs <- levels(factor(Y))
    # Combine the two datasets into a single dataset
    # i.e., under the null hypothesis, there is no difference between the two groups
    # Sample from the combined dataset
    a.random <- data[sample (dim(data)[1], sum(Y == levs[1]), TRUE), ]
    b.random <- data[sample (dim(data)[1], sum(Y == levs[2]), TRUE), ]
    data.random <- rbind(a.random, b.random)
    # Null (permuated) difference
    rand.scat <- scattermat(data.random, Y)
    DSC.random[i] <- sqrt(sum(diag(rand.scat$B))) / sqrt(sum(diag(rand.scat$W)))
  }
  # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference

  pvalue <- round(sum(abs(DSC.random) >= abs(DSC.observed)) / number_of_permutations,4)
  return(list(DSC = round(DSC.observed, 3), pval = pvalue))
}
