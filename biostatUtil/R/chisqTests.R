#' Chi-Squared Tests for independence in contingency tables
#' 
#' The Pearson's Chi-Squared test and likelihood ratio (G test) of independence
#' are performed on the data matrix
#'
#' @param x an object of class \code{CrossTable} containing the contingency table
#'
#' @return A table with method name, test statistic, degrees of freedom, and
#' p-value reported for each Chi-squared test.
#' @author Derek Chiu
#' @seealso \code{\link{CrossTable}}
#' @importFrom Deducer likelihood.test
#' @export
#'
#' @examples
#' # Example from documentation of CrossTable
#' library(descr)
#' data(esoph, package = "datasets")
#' ct <- CrossTable(esoph$alcgp, esoph$agegp, expected = TRUE,
#'                  chisq = FALSE, prop.chisq = FALSE,
#'                  dnn = c("Alcohol consumption", "Tobacco consumption"))
#' chisqTests(ct)
#'                  
#' # Better example
#' set.seed(1108)
#' A <- rbinom(100, 3, 0.2)
#' B <- rbinom(100, 4, 0.8)
#' ct <- CrossTable(A, B)
#' chisqTests(ct)
chisqTests <- function(x) {
  Pearson <- x$CST
  G.test <- Deducer::likelihood.test(x$tab)
  res <- matrix(c(Pearson$statistic, Pearson$parameter, Pearson$p.value,
                  G.test$statistic, G.test$parameter, G.test$p.value,
                  x$total.n, NA, NA), nrow = 3, byrow = T,
                dimnames = list(c("Pearson Chi-Square", "Likelihood Ratio",
                                  "N of Valid"),
                                c("Value", "df", "P-value")))
  return(res)
}
