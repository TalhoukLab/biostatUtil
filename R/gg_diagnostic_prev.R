#' Diagnostic Prevalence
#'
#' Compare PPV or NPV by sensitivity, specificity, and prevalence.
#'
#' `gg_diagnostic_prev` plots positive predictive value (PPV) or negative
#' predictive value (NPV) as a function of specificity, with sensitivity
#' separated by line types and prevalence shown across facets.
#'
#' `gg_prev_fixed` plots PPV or NPV as a function of prevalence, with coloured
#' lines separating fixed pairs of sensitivity and specificity for the lower
#' bound, pooled estimate, and upper bound.
#'
#' @param se a vector of sensitivity estimates. For `gg_prev_fixed`, must be a
#'   vector of length 3 for lower bound, pooled estimate, and upper bound.
#' @param sp a vector of specificity estimates. For `gg_prev_fixed`, must be a
#'   vector of length 3 for lower bound, pooled estimate, and upper bound. For
#'   `gg_diagnostic_prev`, a sequence of values to plot along x-axis.
#' @param p a vector of prevalence values. For `gg_prev_fixed`, must be a
#'   sequence of values to plot along x-axis.
#' @param result whether to show the PPV or NPV on the y-axis
#' @return Graph of PPV or NPV by specificity, sensitivity, and prevalence.
#' @author Derek Chiu
#' @export
#' @examples
#' se <- c(0.5, 0.7, 0.9)
#' sp <- seq(0.21, 1, 0.01)
#' p <- c(0.32, 0.09, 0.026)
#' gg_diagnostic_prev(se, sp, p, result = "PPV")
#' gg_diagnostic_prev(se, sp, p, result = "NPV")
gg_diagnostic_prev <- function(se, sp, p, result = c("PPV", "NPV")) {
  result <- match.arg(result)
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package \"scales\" is needed. Please install it.",
         call. = FALSE)
  } else {
    points <- length(sp)
    df <- data.frame(
      se = rep(se, each = points * 3),
      sp = rep(sp, 9),
      p =  rep(p, length = points * 9)
    ) %>%
      dplyr::mutate(
        ppv = ppv(se, sp, p, 4),
        npv = npv(se, sp, p, 4),
        se = factor(se) %>%
          factor(labels = scales::percent(as.numeric(levels(.)))),
        p = factor(p) %>%
          factor(labels = paste0("(", letters[seq_along(levels(.))], ") ",
                                 scales::percent(as.numeric(levels(.))), " prevalence"))
      )
  }
  if (result == "PPV") {
    var <- rlang::quo(ppv)
  } else {
    var <- rlang::quo(npv)
  }
  ggplot(df, aes(x = sp, y = {{ var }}, linetype = se)) +
    geom_line() +
    scale_x_continuous(labels = scales::percent) +
    facet_wrap(vars(p), nrow = 1) +
    labs(
      x = "Specificity",
      y = result,
      linetype = "Sensitivity"
    ) +
    theme_bw() +
    theme(panel.spacing = unit(1, "lines"))
}

#' @rdname gg_diagnostic_prev
#' @export
#' @examples
#'
#' gg_prev_fixed(
#'   se = c(0.67, 0.83, 0.97),
#'   sp = c(0.86, 0.94, 0.99),
#'   p = seq(0.01, 0.30, 0.01),
#'   result = "PPV"
#' )
gg_prev_fixed <- function(se, sp, p, result = c("PPV", "NPV"),
                          layout = c("lines", "band")) {
  layout <- match.arg(layout)
  points <- length(p)
  df <- data.frame(
    se = rep(se, each = points),
    sp = rep(sp, each = points),
    p =  rep(p, 3)
  ) %>%
    dplyr::mutate(
      ppv = ppv(se, sp, p, 4),
      npv = npv(se, sp, p, 4),
      point = rep(c("lower bound", "pooled estimate", "upper bound"), each = points),
      label = forcats::fct_rev(paste0(.data$point, " (se: ", se, ", sp: ", sp, ")"))
    )
  if (result == "PPV") {
    var <- rlang::quo(ppv)
  } else {
    var <- rlang::quo(npv)
  }
  if (layout == "lines") {
    ggplot(df, aes(x = p, y = {{ var }}, color = .data$label, linetype = .data$label)) +
      scale_x_continuous(n.breaks = 8) +
      scale_linetype_manual(values = c(2, 1, 2)) +
      geom_line(linewidth = 1) +
      labs(
        x = "Prevalence",
        y = result,
        title = paste(result, "vs. Prevalence for Fixed Sensitivity/Specificity")
      ) +
      theme_bw() +
      theme(legend.title = element_blank(),
            panel.grid.minor = element_blank())
  } else if (layout == "band") {
    tmp1 <- df %>%
      tidyr::pivot_wider(id_cols = "p",
                         names_from = "point",
                         values_from = !!var)
    tmp2 <- df %>%
      dplyr::distinct(label) %>%
      dplyr::mutate(aes = ifelse(grepl("bound", label), "fill", "color")) %>%
      tidyr::pivot_wider(
        names_from = "aes",
        values_from = "label",
        values_fn = ~ paste(., collapse = "\n")
      )
    df2 <- dplyr::bind_cols(tmp1, tmp2)

    ggplot(df2,
           aes(
             x = p,
             y = `pooled estimate`,
             ymin = `lower bound`,
             ymax = `upper bound`
           )) +
      geom_ribbon(
        aes(fill = fill),
        color = "orange",
        alpha = 0.3,
        linetype = 2,
        linewidth = 1
      ) +
      geom_line(aes(color = color), linewidth = 1) +
      scale_x_continuous(n.breaks = 8) +
      scale_color_manual(NULL, values = "blue") +
      scale_fill_manual(NULL, values = "orange") +
      labs(
        x = "Prevalence",
        y = result,
        title = paste(result, "vs. Prevalence for Fixed Sensitivity/Specificity")
      ) +
      theme_bw() +
      theme(
        legend.title = element_blank(),
        panel.grid.minor = element_blank(),
        legend.margin = margin(-0.1, 0.2,-0.1, 0.2, unit = "cm")
      )
  }
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
