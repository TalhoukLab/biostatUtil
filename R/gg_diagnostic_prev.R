#' Diagnostic Prevalence
#'
#' Compare PPV or NPV by sensitivity, specificity, and prevalence.
#'
#' A graph of positive predictive value (PPV) or negative predictive value (NPV)
#' is shown a function of specificity. Sensitivity is separated by line types,
#' and prevalence is shown as different facets.
#'
#' @param data input data with metrics
#' @param se column name for sensitivity
#' @param sp column name for specificity
#' @param p column name for prevalence
#' @param result whether to return the PPV or NPV
#' @return A graph of the NPV or PPV by specificity, sensitivty, and prevalence.
#' @author Derek Chiu
#' @export
#' @examples
#' df <- data.frame(
#'   se = rep(c(0.5, 0.7, 0.9), each = 240),
#'   sp = rep(seq(0.21, 1, 0.01), 9),
#'   p = rep(c(0.32, 0.09, 0.026), length = 720)
#' )
#' gg_diagnostic_prev(df, se, sp, p, result = "PPV")
#' gg_diagnostic_prev(df, se, sp, p, result = "NPV")
gg_diagnostic_prev <- function(data, se, sp, p, result = c("PPV", "NPV")) {
  result <- match.arg(result)
  df <- data %>%
    dplyr::mutate(
      ppv = ppv({{ se }}, {{ sp }}, {{ p }}),
      npv = npv({{ se }}, {{ sp }}, {{ p }}),
      se = factor({{ se }}) %>%
        forcats::fct_relabel(~ scales::percent(as.numeric(.))),
      p = factor({{ p }}) %>%
        forcats::fct_relabel(~ scales::percent(as.numeric(.))) %>%
        forcats::fct_relabel(~ paste0("(", letters[seq_along(.)], ") ", ., " prevalence"))
    )
  if (result == "PPV") {
    var <- rlang::quo(ppv)
  } else {
    var <- rlang::quo(npv)
  }
  ggplot(df, aes(x = {{ sp }}, y = {{ var }}, linetype = {{ se }})) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    facet_wrap(vars({{ p }}), nrow = 1) +
    labs(
      x = "Specificity",
      y = result,
      linetype = "Sensitivity"
    ) +
    theme(panel.spacing = unit(1, "lines"))
}

#' Calculate PPV as a function of sensitivity, specificty, and prevalence
#'
#' @noRd
ppv <- function(se, sp, p, digits = 2) {
  ppv <- (se * p) / ((se * p) + ((1 - sp) * (1 - p)))
  return(round(ppv, digits))
}

#' Calculate NPV as a function of sensitivity, specificty, and prevalence
#'
#' @noRd
npv <- function(se, sp, p, digits = 2) {
  npv <- sp * (1 - p) / (sp * (1 - p) + (1 - se) * p)
  return(round(npv, digits))
}
