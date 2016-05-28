#' Nice output from Cox regression object
#' 
#' Nicely formatted output from a Cox regression object,
#' either \code{coxph} or \code{coxphf}.
#' 
#' For objects of class \code{coxphf}, the calculation of the number of events used
#' in the fit is slightly different.
#' 
#' @param object a model fit object returned from \code{coxph} or \code{coxphf}
#' @param coefnames a vector of labels for the coefficient names returned by the fit.
#' \code{NULL} by default, uses original coefficient names.
#' @param conf.level confidence level for hazard ratio. Defaults to 95\%.
#' @param digits number of digits to round to
#' @return A matrix with a row for each coefficient, and a column for each of the following
#' elements
#' \item{n}{the number of observations used in the fit.}
#' \item{events}{the number of events used in the fit.}
#' \item{coef}{log hazard ratio}
#' \item{se}{standard error of \code{coef}}
#' \item{Z-Score}{Wald test statistic}
#' \item{P-value}{p-value for \code{Z-Score}}
#' \item{HR}{hazard ratio}
#' \item{Lower CI Limit}{Defaults to \code{2.5 \%}}
#' \item{Upper CI Limit}{Defaults to \code{97.5 \%}}
#' @author Aline Talhouk, Derek Chiu
#' @importFrom stats confint
#' @export
#' @examples
#' test1 <- list(time=c(4,3,1,1,2,2,3), 
#' status=c(1,1,1,0,1,1,0), 
#' x=c(0,2,1,1,1,0,0), 
#' sex=c(0,0,0,0,1,1,1)) 
#' 
#' # Stratified
#' mod1 <- coxph(Surv(time, status) ~ x + strata(sex), test1) 
#' coxphOut(mod1)
#' 
#' # Not stratified
#' mod2 <- coxph(Surv(time, status) ~ x + sex, test1) 
#' coxphOut(mod2, coefnames = c("x", "gender"))
coxphOut <- function(object, coefnames = NULL, conf.level = 0.95,
                     digits = 2) {
  cox <- object
  n <- cox$n
  events <- ifelse("coxphf" %in% class(cox),
                   sum(cox$y[, "status"]), cox$nevent)
  coef <- cox$coef
  se <- sqrt(diag(cox$var))
  z <- coef / se
  p <- 1 - pchisq(z^2, 1)
  HR <- exp(coef)
  CI <- exp(confint(cox, level = conf.level))
  tmp <- cbind(n, events, coef, se, "Z-Score" = z, "P-value" = p, HR, CI)
  if (!is.null(coefnames))
    rownames(tmp) <- coefnames
  return(round(tmp, digits))  
}