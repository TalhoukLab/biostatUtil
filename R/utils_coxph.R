#' Nice output from Cox regression object
#'
#' Nicely formatted output from a Cox regression object, either `coxph` or
#' `coxphf`.
#'
#' For objects of class `coxphf`, the calculation of the number of events
#' used in the fit is slightly different.
#'
#' @param object a model fit object returned from `coxph` or `coxphf`
#' @param coefnames a vector of labels for the coefficient names returned by the
#'   fit. `NULL` by default, uses original coefficient names.
#' @param conf.level confidence level for hazard ratio. Defaults to 95%.
#' @param digits number of digits to round to
#' @return A matrix with a row for each coefficient, and a column for each of
#'   the following elements
#' \item{n}{the number of observations used in the fit.}
#' \item{events}{the number of events used in the fit.}
#' \item{coef}{log hazard ratio}
#' \item{se}{standard error of `coef`}
#' \item{Z-Score}{Wald test statistic}
#' \item{P-value}{p-value for `Z-Score`}
#' \item{HR}{hazard ratio}
#' \item{Lower CI Limit}{Defaults to `2.5 \%`}
#' \item{Upper CI Limit}{Defaults to `97.5 \%`}
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples
#' library(survival)
#' test1 <- list(time = c(4, 3, 1, 1, 2, 2, 3),
#' status = c(1, 1, 1, 0, 1, 1, 0),
#' x = c(0, 2, 1, 1, 1, 0, 0),
#' sex = c(0, 0, 0, 0, 1, 1, 1))
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
  events <- ifelse("coxphf" %in% class(cox), sum(cox$y[, "status"]), cox$nevent)
  coef <- cox$coef
  se <- sqrt(diag(cox$var))
  z <- coef / se
  p <- 1 - stats::pchisq(z ^ 2, 1)
  HR <- exp(coef)
  CI <- exp(stats::confint(cox, level = conf.level))
  tmp <- cbind(n, events, coef, se, "Z-Score" = z, "P-value" = p, HR, CI)
  if (!is.null(coefnames))
    rownames(tmp) <- coefnames
  return(round(tmp, digits))
}

#' Univariate cox proprtional hazards model
#'
#' Concatenates hazard ratios and confidence limits for every covariate in a Cox
#' model.
#'
#' @param mod model fit object, returned from either `coxph` or
#'   `coxphf`.
#' @param digits number of digits to round
#' @return Hazard ratio and 95/% confidence interval
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples
#' library(survival)
#' library(coxphf)
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
  UseMethod("Xunivcoxph")
}

#' @export
Xunivcoxph.coxph <- function(mod, digits = 3) {
  mod %>%
    broom::tidy(exponentiate = TRUE, conf.int = TRUE) %>%
    magrittr::extract(c("estimate", "conf.low", "conf.high")) %>%
    format_hr_ci(digits) %>%
    paste0("HR = ", .)
}

#' @export
Xunivcoxph.coxphf <- function(mod, digits = 3) {
  mod %>%
    magrittr::extract(c("coefficients", "ci.lower", "ci.upper")) %>%
    purrr::map_at("coefficients", exp) %>%
    format_hr_ci(digits) %>%
    paste0("HR^(F) = ", .)
}

#' @noRd
as_plotmath <- function(x) {
  x %>%
    gsub("%", "*'%'*", .) %>%
    gsub("&", "*'&'*", .) %>%
    gsub(">", "*'>'*", .) %>%
    gsub("<", "*'<'*", .) %>%
    gsub("=", "*'='*", .) %>%
    gsub("[+]", "*'+'*", .) %>%
    gsub("-", "*'-'*", .) %>%
    gsub("\u2014", "*'\u2014'*", .) %>%
    gsub("/", "*'/'*", .) %>%
    gsub(",", "*','*", .) %>%
    gsub("[(]", "*'('*", .) %>%
    gsub("[)]", "*')'*", .) %>%
    gsub("\\{", "*'{'*", .) %>%
    gsub("\\}", "*'}'*", .) %>%
    gsub("\\[", "*'['*", .) %>%
    gsub("\\]", "*']'*", .) %>%
    gsub("*'('*F*')'*", "'(F)'", ., fixed = TRUE) %>%
    gsub("\\*{2}", "\\*", .) %>%
    gsub("[*]? ~ [*]?", "%~%", .) %>%
    gsub("[*]? [*]?", "~", .) %>%
    gsub("^[*]|[*]$", "", .) %>%
    gsub("([[:digit:]]{1})([[:alpha:]]{1})", "\\1*\\2", .)
}

#' @noRd
format_hr_ci <- function(stats, digits, labels = TRUE, method = c("Inf", "Sci")
                         ) {
  stats %>%
    purrr::map(round, digits) %>%
    purrr::map(sprintf, fmt = paste0("%.", digits, "f")) %>%
    unname() %>%
    purrr::pmap_chr(paste_hr_ci, labels = labels, method = method)
}

#' @noRd
paste_hr_ci <- function(hr, ci.lo, ci.hi, labels = TRUE,
                        method = c("Inf", "Sci")) {
  method <- match.arg(method)
  hr_ci <- list(hr, ci.lo, ci.hi) %>%
    as.numeric() %>%
    purrr::map_if(
      .p = ~ is.na(.) || . > 1000,
      .f = ~ switch(method,
                    `Inf` = "Inf",
                    Sci = format(., digits = 3, scientific = TRUE))
    )
  paste0(hr_ci[[1]], ifelse(labels, " (95% CI: ", " ("),
         hr_ci[[2]], "-", hr_ci[[3]], ")")
}

#' Create a survival formula from time, status, event code and terms strings
#' Left-truncated survival can be used by inserting a time2 arg
#' @noRd
surv_formula <- function(time, status, event, terms, time2 = NULL,
                         strata = NULL) {
  if (is.null(time2)) {
    response <-
      paste0("Surv(", time, ", ", status, " == '", event, "')")
  } else {
    response <-
      paste0("Surv(", time, ", ", time2, ", ", status, " == '", event, "')")
  }
  if (is.null(strata)) {
    covariates <- paste(rlang::syms(terms), collapse = " + ")
  } else {
    covariates <-
      paste(c(rlang::syms(terms), paste0("strata(", strata, ")")), collapse = " + ")
  }
  stats::as.formula(paste(response, covariates, sep = " ~ "))
}

#' Round p-values according to specifications for coxph summaries
#' @noRd
round_pval <- function(pvalue, round.small = TRUE, scientific = FALSE,
                       digits = 4) {
  if (round.small) {
    p <- round_small(pvalue, method = "round", digits = digits,
                     sci = scientific)
    if (grepl("<", p)) {
      p
    } else {
      sprintf(paste0("%.", digits, "f"), p)
    }
  } else {
    sprintf(paste0("%.", digits, "f"), round(pvalue, digits = digits))
  }
}

#' Date of censoring
#'
#' Calculate date of censoring using date of diagnosis and last follow-up.
#'
#' The cutoff date is `cutoff_yr` years after the date of diagnosis, on December
#' 31st. If the cutoff date is earlier than the date of last follow-up, then the
#' cutoff date is the new date of censoring, otherwise use the date of last
#' follow-up.
#'
#' @param date_dx date of diagnosis
#' @param date_lfu date of last follow-up
#' @param cutoff_yr number of years to cutoff at
#' @return date of censoring
#' @author Derek Chiu
#'
#' @export
#' @examples
#' date_dx <- as.Date("2003-03-08")
#' date_lfu <- as.Date("2009-08-09")
#' censor_date(date_dx, date_lfu, 5)
#' censor_date(date_dx, date_lfu, 6)
censor_date <- function(date_dx, date_lfu, cutoff_yr) {
  date_cutoff <-
    lubridate::make_date(lubridate::year(date_dx) + cutoff_yr, 12, 31)
  dplyr::if_else(date_cutoff < date_lfu, date_cutoff, date_lfu)
}
