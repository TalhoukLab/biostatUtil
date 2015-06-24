#' Univariate cox proprtional hazards model
#' @param mod model fit object, returned from either \code{coxph}
#' or \code{coxphf}.
#' @param coxph.type model fit type; either \code{coxph} (default)
#' or \code{coxphf} (Firth's correction).
#' @param digits number of digits to round
#' @return Hazard ratio, confidence interval, and likelihood ratio test
#' p-value
#' @author Aline Talhouk
#' @export
Xunivcoxph <- function(mod, coxph.type = "coxph", digits = 3) {
  switch(coxph.type,
         coxph = {
           HR <- round(summary(mod)$conf.int, digits)[, 1:2]
           CI <- paste(round(summary(mod)$conf.int, digits)[, 3],
                       round(summary(mod)$conf.int, digits)[, 4], sep = "-")
           if (is.null(dim(HR))) {
             HR <- HR[1]  # when there are only two groups
           } else {
             HR <- HR[, 1]  # when there are > 2 groups
           }
           res <- paste0("HR ", HR, " (95% CI, ", CI, ")")
         },
         coxphf = {
           HR <- round(exp(mod$coefficients), digits)
           CI <- paste(round(mod$ci.lower, digits),
                       round(mod$ci.upper, digits), sep = "-")
           res <- paste0("HR(F) ", HR, " (95% CI, ", CI, ")")
         }
  )
  return(res)
}