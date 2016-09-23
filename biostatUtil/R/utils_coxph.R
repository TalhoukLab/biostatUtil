#' Univariate cox proprtional hazards model
#' 
#' @param mod model fit object, returned from either \code{coxph} or
#'   \code{coxphf}.
#' @param digits number of digits to round
#' @return Hazard ratio and 95/% confidence interval
#' @author Aline Talhouk, Derek Chiu
#' @export
#' # One predictor
#' test1 <- list(
#'   time = c(4, 3, 1, 1, 2, 2, 3),
#'   status = c(1, 1, 1, 0, 1, 1, 0),
#'   x = c(0, 2, 1, 1, 1, 0, 0),
#'   sex = c(0, 0, 0, 0, 1, 1, 1)
#' )
#' mod <- coxph(Surv(time, status) ~ x + strata(sex), test1)
#' Xunivcoxph(mod)
#' 
#' # Multiple predictors
#' bladder1 <- bladder[bladder$enum < 5, ] 
#' mod <- coxph(Surv(stop, event) ~ (rx + size + number) * strata(enum) + 
#' cluster(id), bladder1)
#' Xunivcoxph(mod, digits = 2)
#' 
#' # Firth's correction
#' test2 <- data.frame(list(
#'   start = c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
#'   stop = c(2, 3, 6, 7, 8, 9, 9, 9, 14, 17),
#'   event = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
#'   x    = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0)
#' ))
#' mod <- coxphf(formula = Surv(start, stop, event) ~ x, pl = FALSE,
#' data = test2)
#' Xunivcoxph(mod)
Xunivcoxph <- function(mod, digits = 3) {
  if (as.character(mod$call[1]) == "coxph") {
    mod.summ <- round(summary(mod)$conf.int, digits)
    HR <- mod.summ[, 1, drop = FALSE]
    CI <- paste(mod.summ[, 3], mod.summ[, 4], sep = "-")
    res <- paste0("HR ", HR, " (95% CI, ", CI, ")")
    return(res)
  } else if (as.character(mod$call[1]) == "coxphf") {
    HR <- round(exp(mod$coefficients), digits)
    CI <- paste(round(mod$ci.lower, digits), round(mod$ci.upper, digits),
                sep = "-")
    res <- paste0("HR(F) ", HR, " (95% CI, ", CI, ")")
    return(res)
  } else {
    stop("'mod' must an object of class 'coxph' or 'coxphf'.")
  }
}