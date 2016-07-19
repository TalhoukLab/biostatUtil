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
  Pearson.obj <- c(Pearson$statistic, Pearson$parameter, Pearson$p.value)
  G.test <- Deducer::likelihood.test(x$tab)
  G.test.obj <- c(G.test$statistic, G.test$parameter, G.test$p.value)
  
  Fisher <- x$fisher.ts
  if (!any(is.na(Fisher))) {
    Fisher.obj <- c(NA, NA, Fisher$p.value)
  } else {
    Fisher.obj <- rep(NA, 3)
  }
  
  LBL <- tryCatch(coin::lbl_test(x$tab), error = function(e) return(NULL))
  if (!is.null(LBL)) {
    LBL.obj <- c(coin::statistic(LBL), 1, coin::pvalue(LBL))  
  } else {
    LBL.obj <- rep(NA, 3)
  }
  res <- data.frame(Pearson.obj, G.test.obj, Fisher.obj, LBL.obj) %>% 
    t() %>% 
    as.data.frame() %>% 
    magrittr::set_colnames(c("Value", "df", "P-value")) %>% 
    mutate_each(funs(round(., digits)), 1:2) %>%
    mutate(`P-value` = sapply(`P-value`, round_small, digits)) %>%
    magrittr::set_rownames(c("Pearson Chi-Square",
                             "Likelihood Ratio",
                             "Fisher's Exact Test",
                             "Linear-by-Linear Association")) %>% 
    rbind(., "N of Valid Cases" = c(x$gt, "", ""))
  return(res)
}
