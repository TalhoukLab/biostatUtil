#' Dispersion Separability Criterion (DSC)
#'
#' Compute the DSC which indicates how similar two data sets are by considering
#' the between data scatter matrix and the withing data scatter matrix.
#'
#' First, the different batches in \code{data} are combined into a single dataset. Under
#' the null hypothesis, there is no difference between the two groups. We sample from the
#' combined dataset, then compute the null (permuted) difference.
#' The p-value in the permutation test is the fraction of how times the permuted
#' difference is equal to or more extreme than the observed difference.
#'
#' @param data a combined data set
#' @param Y vector indicating the two different batches
#' @param permut number of permutations to compute a pvalue for DSC using a permutation test.
#' @param digits.d number of decimal places for DSC
#' @param digits.p number of decimal places for p-value
#' @return A list with elements
#' \item{DSC}{Dispersion Separability Criterion}
#' \item{pval}{Permutation test p-value}
#' @author Aline Talhouk
#' @export
#' @examples
#' set.seed(4)
#' dat <- matrix(rnorm(100), ncol = 10)
#' batches <- sample(1:2, 10, replace = TRUE)
#' DSC(dat, batches, permut = 1000)
DSC <- function(data, Y, permut = 10, digits.d = 3,
                digits.p = floor(log10(permut)) + 1) {
  scatmat <- scattermat(data, Y)
  DSC.observed <- sqrt(sum(diag(scatmat$B))) / sqrt(sum(diag(scatmat$W)))
  DSC.random <- NULL
  for (i in 1:permut) {
    levs <- levels(factor(Y))
    a.random <- data[sample(dim(data)[1], sum(Y == levs[1]), TRUE), ]
    b.random <- data[sample(dim(data)[1], sum(Y == levs[2]), TRUE), ]
    data.random <- rbind(a.random, b.random)
    rand.scat <- scattermat(data.random, Y)
    DSC.random[i] <- sqrt(sum(diag(rand.scat$B))) /
      sqrt(sum(diag(rand.scat$W)))
  }
  pvalue <- sum(abs(DSC.random) >= abs(DSC.observed)) / permut
  return(list(DSC = round(DSC.observed, digits = digits.d),
              pval = round(pvalue, digits = digits.p)))
}
