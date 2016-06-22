#' Chi-Squared Tests for Independence in Contingency Tables
#' 
#' The Pearson's Chi-Squared test, likelihood ratio (G test) of independence,
#' and linear-by-linear association test are performed on the data matrix.
#'
#' @param x an object of class \code{CrossTable} containing the contingency table
#' @param digits number of digits to round to
#'
#' @return A table with method name, test statistic, degrees of freedom, and
#' p-value reported for each Chi-squared test.
#' @author Derek Chiu
#' @seealso \code{\link{CrossTable}}
#' @importFrom Deducer likelihood.test
#' @importFrom coin lbl_test statistic pvalue
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
chisqTests <- function(x, digits = 3) {
  . <- NULL
  Pearson <- x$CST
  G.test <- Deducer::likelihood.test(x$tab)
  LBL <- coin::lbl_test(x$tab)
  res <- matrix(c(Pearson$statistic, Pearson$parameter, Pearson$p.value,
                  G.test$statistic, G.test$parameter, G.test$p.value,
                  coin::statistic(LBL), 1, coin::pvalue(LBL)), ncol = 3, byrow = T,
                dimnames = list(c("Pearson Chi-Square", "Likelihood Ratio",
                                  "Linear-by-Linear Association"),
                                c("Value", "df", "P-value"))) %>% 
    round(digits) %>% 
    rbind(., "N of Valid Cases" = c(x$total.n, "", ""))
  return(res)
}
