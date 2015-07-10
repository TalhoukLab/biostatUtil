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
#' @export
coxphOut <- function (object, coefnames = NULL, conf.level = 0.95, digits = 2) {
  cox <- object
  Coef <- cox$coef
  se <- sqrt(diag(cox$var))
  CI <- exp(confint(cox, level = level))
  n <- cox$n
  events <- ifelse("coxphf" %in% class(cox),
                   sum(cox$y[, "status"]), cox$nevent)
  Z <- Coef / se
  p <- 1 - pchisq(Z^2, 1)
  tmp <- cbind(n, events, Coef, se, Z, p, HR = exp(Coef), CI)
  if(is.null(coefnames))
    coefnames = names(Coef)
  dimnames(tmp) <- list(coefnames, c("n", "events" , "coef ", " se ",
                                     " Z-Score ", " P-value ", " HR ",
                                     colnames(CI)))
  return(round(tmp, digits))  
}