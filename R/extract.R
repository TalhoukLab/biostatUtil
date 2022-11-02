#' Extract results for executive summary
#'
#' Extract statistical test results for Executive Summary section at beginning
#' of an RSF.
#'
#' * `extract_km()` extracts the Kaplan-Meier Log Rank test p-value for a
#' specified variable
#' * `extract_cox()` extracts Cox proportional hazards model hazard ratios with
#' confidence intervals and LRT p-values for a specified variable
#'
#' @param data data frame with clinical variables and outcomes
#' @param var variable name
#' @param outcome survival outcome. One of "os", "dss", "pfs".
#' @param args list of arguments for survival analyses
#' @param digits number of digits to round p-value
#' @name extract_results
#' @author Derek Chiu
#' @export
extract_km <- function(data, var, outcome, args, digits = 3) {
  f <- rlang::new_formula(rlang::expr(survival::Surv(
    !!rlang::parse_expr(args[[outcome]][["time"]]),
    !!rlang::parse_expr(args[[outcome]][["status"]])
  )), rlang::expr(!!rlang::sym(var)))

  survival::survdiff(f, data = data) %>%
    getPval() %>%
    round_small(method = "signif", digits = digits) %>%
    paste0("KM curves, Log rank test p=", .)
}

#' @rdname extract_results
#' @param x an object from result of `doCoxphGeneric()` or `doCoxphMultivariable()`
#' @export
extract_cox <- function(x, var, outcome) {
  res <- x[["result.table"]][paste(var, outcome, sep = "-"), 2:3] %>%
    gsub("</*sup>", "^", .)
  names(res)[1] <- "HR"
  names(res)[2] <- ifelse(
    grepl("<", res[2]),
    gsub("P-value", "p", names(res)[2]),
    gsub("P-value", "p =", names(res)[2])
  )
  res %>%
    purrr::imap_chr(~ paste(.y, .x)) %>%
    paste(collapse = ", ")
}
