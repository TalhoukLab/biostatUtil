#' Sample Size Calculation
#'
#' Sample size calculation using model-based discrimination measure.
#'
#' The sample size calculation uses a discrimination measured based on
#' multivariable prognostic time-to-event models.
#'
#' @param fit an object fit of class `coxph`
#' @param D discrimination measure D, defaults to output of
#'   `survival::royston()` for `fit`
#' @param cens censoring proportion, defaults to proportion calculated from
#'   `fit`
#' @param p proportion of `D` accepted as `delta`, defaults to 10%.
#' @param alpha Type I error
#' @param power statistical power (1 - Type II error)
#' @param delta superiority/non-inferiority margin. The difference we wish to
#' detect between a prior model and a new model.
#' @param trial either "superiority" or "non-inferiority"
#' @return The final sample size needed in the new model using the supplied
#'   input parameters.
#' @author Derek Chiu
#' @references \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4603804/}
#' @export
ssize_D <- function(fit, D = NULL, cens = NULL, p = 0.1, alpha = 0.05,
                    power = 0.8, delta = 0.25,
                    trial = c("superiority", "non-inferiority")) {
  trial <- match.arg(trial)
  beta <- 1 - power
  zz <- switch(
    trial,
    superiority = stats::qnorm(1 - alpha / 2) + stats::qnorm(1 - beta),
    `non-inferiority` = stats::qnorm(1 - alpha) + stats::qnorm(1 - beta)
  )
  royston_D <- survival::royston(fit)
  D <- D %||% royston_D[["D"]]
  cens <- cens %||% (1 - fit$nevent / fit$n)
  lambda_m <- 2.66 + 1.26 * D ^ 1.9 - 1.65 * (D * cens) ^ 1.3 # Equation 3
  e2 <- lambda_m * (delta / zz) ^ -2 # Equation B2
  e <- lambda_m * (p * D / zz) ^ -2 # Equation 4

  # Sample sizes are event sizes multiplied by reciprocal of event rate
  n2 <- e2 / (1 - cens)
  n <- e / (1 - cens)

  # Take the lower of n2 or n (absolute and relative precision)
  n_final <- ceiling(pmin(n2, n))
  n_final
}

#' Convert R2 to D
#'
#' Convert R-Squared to D measure
#'
#' @param R2 R-squared
#' @return The Royston & Sauerbrei D measure
#' @author Derek Chiu
#' @export
R2_to_D <- function(R2) {
  beta <- sqrt((R2 * pi ^ 2) / (6 * (1 - R2)))
  D <- beta * sqrt(8 / pi)
  D
}
