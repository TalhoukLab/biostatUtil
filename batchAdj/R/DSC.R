#' Dispersion Separability Criterion (DSC)
#'
#' The DSC indicates how similar two data sets are by considering
#' the between data scatter matrix and the within data scatter matrix.
#'
#' First, the different batches in \code{data} are combined into a single dataset. Under
#' the null hypothesis, there is no difference between the two groups. We sample from the
#' combined dataset, then compute the null (permuted) difference.
#' The p-value in the permutation test is the fraction of times the permuted
#' difference is equal to or more extreme than the observed difference.
#'
#' @param data a combined data set
#' @param Y vector indicating the two different batches
#' @param permut number of permutations to compute a pvalue for DSC using a permutation test
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
  obs.scat <- scattermat(data, Y)
  DSC.obs <- sqrt(sum(diag(obs.scat$B))) / sqrt(sum(diag(obs.scat$W)))
  DSC.rand <- NULL
  for (i in 1:permut) {
    a.rand <- data[sample(nrow(data), sum(Y == levels(factor(Y))[1]), TRUE), ]
    b.rand <- data[sample(nrow(data), sum(Y == levels(factor(Y))[2]), TRUE), ]
    rand.scat <- scattermat(rbind(a.rand, b.rand), Y)
    DSC.rand[i] <- sqrt(sum(diag(rand.scat$B))) / sqrt(sum(diag(rand.scat$W)))
  }
  pvalue <- sum(abs(DSC.rand) >= abs(DSC.obs)) / permut
  return(list(DSC = round(DSC.obs, digits = digits.d),
              pval = round(pvalue, digits = digits.p)))
}
