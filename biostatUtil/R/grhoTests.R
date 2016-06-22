#' Summary of Survival Curve differences using G-rho tests
#' 
#' Runs the log-rank test and Breslow test to test for difference between
#' two or more survival curves.
#' 
#' The log-rank test corresponds to \code{rho = 0} and the Breslow test corresponds to \code{rho = 1}
#' in \code{survdiff}. This function emulates the "Overall Comparisons" table output from SPSS.
#'
#' @param formula formula expression of the form \code{Surv(time, status) ~ predictors}
#' @param data data frame where variables from \code{formula} originate
#' @param digits number of significant digits to retain
#'
#' @return The Chi-Square statistic, degrees of freedom, and p-value are given
#' for both G-rho tests.
#' @author Derek Chiu
#' @export
#'
#' @examples
#' grhoTests(Surv(futime, fustat) ~ rx, data = ovarian)
grhoTests <- function(formula, data, digits = 4) {
  pos <- 1
  assign("formula", formula, envir = as.environment(pos))
  assign("data", data, envir = as.environment(pos))
  tab <- plyr::ldply(c(0, 1), function(r) {
    mod <- survdiff(formula, data, rho = r)
    c <- mod$chisq
    d <- length(mod$n) - 1
    p <- pchisq(c, d, lower.tail = FALSE)
    res <- data.frame(c, d, p)
    return(res)}) %>%
    signif(digits) %>% 
    set_rownames(c("Log Rank (Mantel-Cox)", "Breslow (Generalized Wilcoxon)")) %>% 
    set_colnames(c("Chi-Square", "df", "Sig."))
  return(tab)
}
