#' Kaplan-Meier Plots using ggplot
#'
#' Produce nicely annotated KM plots using ggplot style.
#'
#' @param sfit an object of class `survfit` containing one or more survival
#'   curves
#' @param sfit2 an (optional) second object of class `survfit` to compare
#'   with `sfit`
#' @param table logical; if `TRUE` (default), the numbers at risk at each
#'   time of death is shown as a table underneath the plot
#' @param returns logical; if `TRUE` the plot is returned
#' @param marks logical; if `TRUE` (default), censoring marks are shown on
#'   survival curves
#' @param CI logical; if `TRUE` (default), confidence bands are drawn for
#'   survival curves the using cumulative hazard, or log(survival).
#' @param line.pattern linetype for survival curves
#' @param shading.colors vector of colours for each survival curve
#' @param main plot title
#' @param xlabs horizontal axis label
#' @param ylabs vertical axis label
#' @param xlims horizontal limits for plot
#' @param ylims vertical limits for plot
#' @param ystratalabs labels for the strata being compared in `survfit`
#' @param cox.ref.grp indicates reference group for the variable of interest in
#'   the cox model.  this parameter will be ignored if not applicable, e.g. for
#'   continuous variable
#' @param timeby length of time between consecutive time points spanning the
#'   entire range of follow-up. Defaults to 5.
#' @param test type of test. Either "Log Rank" (default) or "Tarone-Ware".
#' @param pval logical; if `TRUE` (default), the logrank test p-value is
#'   shown on the plot
#' @param bold_pval logical; if `TRUE`, p-values are bolded if statistically
#'   significant at `sig.level`
#' @param sig.level significance level; default 0.05
#' @param HR logical; if `TRUE` (default), the estimated hazard ratio and
#'   its 95% confidence interval will be shown
#' @param use.firth Firth's method for Cox regression is used if the percentage
#'   of censored cases exceeds `use.firth`. Setting `use.firth = 1`
#'   (default) means Firth is never used, and `use.firth = -1` means Firth
#'   is always used.
#' @param hide.border logical; if `TRUE`, the upper and right plot borders are
#'   not shown.
#' @param expand.scale logical; if `TRUE` (default), the scales are expanded by
#'   5% so that the data are placed a slight distance away from the axes
#' @param legend logical; if `TRUE`, the legend is overlaid on the graph
#'   (instead of on the side).
#' @param legend.xy named vector specifying the x/y position of the legend
#' @param legend.direction layout of items in legends ("horizontal" (default) or
#'   "vertical")
#' @param line.y.increment how much y should be incremented for each line
#' @param size.plot text size of main plot
#' @param size.summary text size of numerical summaries
#' @param size.table text size of risk table
#' @param size.table.labels text size of risk table axis labels
#' @param digits number of digits to round: p-values digits=number of
#'   significant digits, HR digits=number of digits after decimal point NOT
#'   significant digits
#' @param ... additional arguments to other methods
#' @return A kaplan-meier plot with optional annotations for hazard ratios, log
#'   rank test p-values, and risk table counts for each stratum.
#'
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples
#' library(survival)
#' sfit <- survfit(Surv(time, status) ~ sex, lung)
#' ggkm(sfit, timeby = 200, main = "Survival curves by sex")
ggkm <- function(sfit, sfit2 = NULL, table = TRUE, returns = TRUE, marks = TRUE,
                 CI = TRUE, line.pattern = NULL, shading.colors = NULL,
                 main = "Kaplan-Meier Plot", xlabs = "Time",
                 ylabs = "Survival Probability", xlims = NULL, ylims = NULL,
                 ystratalabs = NULL, cox.ref.grp = NULL, timeby = 5,
                 test = c("Log Rank", "Tarone-Ware"),
                 pval = TRUE, bold_pval = FALSE, sig.level = 0.05,
                 HR = TRUE, use.firth = 1, hide.border = FALSE, expand.scale = TRUE,
                 legend = FALSE, legend.xy = NULL, legend.direction = "horizontal",
                 line.y.increment = 0.05, size.plot = 11, size.summary = 3,
                 size.table = 3.5, size.table.labels = 12, digits = 3, ...) {
  test <- match.arg(test)
  if (test == "Tarone-Ware" && !requireNamespace("coin", quietly = TRUE)) {
    stop("Package \"coin\" is required for Tarone-Ware test. Please install it.",
         call. = FALSE)
  }
  time <- surv <- lower <- upper <- n.censor <- n.risk <- n.event <-
    estimate <- conf.high <- conf.low <- NULL
  times <- seq.int(0, max(sfit$time), by = timeby)
  s <- summary(sfit, censored = TRUE)$strata

  # Specifying plot parameter defaults
  shading.colors <- shading.colors %||% c("blue2", "red2",
                                          "deepskyblue", "indianred3")
  shading.colors <- utils::head(shading.colors, nlevels(s))
  xlims <- xlims %||% c(0, max(sfit$time))
  ylims <- ylims %||% c(0, 1)
  legend.xy <- legend.xy %||% c(0.8, 0.88)
  ystratalabs <- ystratalabs %||% gsub(".*=(.)", "\\1", names(sfit$strata))
  if (is.null(line.pattern) | length(line.pattern) == 1)
    line.pattern <- stats::setNames(rep(1, length(sfit$strata)), ystratalabs)
  if (!is.null(cox.ref.grp))
    names(cox.ref.grp) <- all.vars(sfit$call)[3]  # works for two-sided formulas
  if (expand.scale) {
    expand <- waiver()
    lab.offset <- 0.01
  } else {
    expand <- c(0, 0)
    lab.offset <- 0.05
  }

  # Data for KM plot
  .df <- sfit %>%
    broom::tidy() %>%
    dplyr::select(time, n.risk, n.event, n.censor, surv = estimate,
                  strata, upper = conf.high, lower = conf.low) %>%
    dplyr::mutate(strata = factor(s, labels = ystratalabs)) %>%
    dplyr::bind_rows(
      data.frame(time = 0, surv = 1,
                 strata = factor(ystratalabs, levels = levels(.$strata)),
                 upper = 1, lower = 1), .)

  # KM plot
  p <- ggplot(.df, aes(time, surv, color = strata, fill = strata,
                       linetype = strata)) +
    geom_step(linewidth = 0.7) +
    scale_colour_manual(values = shading.colors) +
    scale_fill_manual(values = shading.colors) +
    scale_linetype_manual(values = line.pattern) +
    scale_x_continuous(xlabs, breaks = times, limits = xlims, expand = expand) +
    scale_y_continuous(ylabs, limits = ylims, expand = expand) +
    theme_bw() +
    theme(
      text = element_text(size = size.plot),
      axis.title.x = element_text(vjust = 0.5),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none"
    ) +
    ggtitle(main)
  if (hide.border)  # Suppress upper and right plot borders
    p <- p + theme(axis.line = element_line(color = "black"),
                   panel.border = element_blank())
  if (legend)  # Legend
    p <- p + theme(legend.position = legend.xy,
                   legend.key = element_rect(colour = NA),
                   legend.background = element_rect(fill = NA),
                   legend.title = element_blank(),
                   legend.direction = legend.direction)
  if (CI)  # Confidence Bands
    p <- p + geom_ribbon(data = .df, aes(ymin = lower, ymax = upper),
                         alpha = 0.05, linetype = 0)
  if (marks)  # Censor Marks
    p <- p + geom_point(data = subset(.df, n.censor >= 1),
                        aes(x = time, y = surv), shape = "/", size = 4)

  # HR statistic (95% CI), log rank test p-value for sfit (or sfit2, if exists)
  if (pval && length(sfit$strata) > 1) {
    fit <- sfit2 %||% sfit
    p <- summarize_km(
      fit = fit,
      p = p,
      test = test,
      digits = digits,
      bold_pval = bold_pval,
      sig.level = sig.level,
      HR = HR,
      cox.ref.grp = cox.ref.grp,
      use.firth = use.firth,
      ystratalabs = ystratalabs,
      line.y.increment = line.y.increment,
      size.summary = size.summary,
      lab.offset = lab.offset
    )
  }

  # Create table graphic to include at-risk numbers, keep at-risk numbers
  # same order as appears in HR (do not reverse levels)
  if (table) {
    risk.data <- sfit %>%
      summary(times = times, extend = TRUE) %>%
      list() %>%
      purrr::map_df(`[`, c("strata", "time", "n.risk"))
    ystratalabs_md <-
      stringr::str_replace_all(ystratalabs, c("<" = "&lt;", ">" = "&gt;"))
    data.table <- ggplot(risk.data, aes(x = time, y = strata,
                                        label = format(n.risk, nsmall = 0))) +
      geom_text(size = size.table) +
      scale_y_discrete(labels = ystratalabs_md) +
      scale_x_continuous(limits = xlims) +
      theme_bw() +
      theme(
        plot.title = element_text(size = size.table.labels),
        text = element_text(size = size.table.labels),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_markdown(color = shading.colors,
                                       face = "bold",
                                       hjust = 1),
        legend.position = "none"
      ) +
      labs(y = NULL, title = "Number at risk")
    if (returns) {
      p <- patchwork::wrap_plots(p, data.table, heights = c(4, 1))
      plot(p)
    }
  } else {
    return(p)
  }
}

#' Numerical summaries of km fit: HR (95\% CI), Log rank test p-value
#' @noRd
summarize_km <- function(fit, p, test, digits, bold_pval, sig.level, HR, cox.ref.grp,
                         use.firth, ystratalabs, line.y.increment, size.summary,
                         lab.offset) {
  f <- eval(fit$call$formula)
  d <- eval(fit$call$data)
  res <- switch(
    test,
    `Log Rank` = getPval(survdiff(f, d)),
    `Tarone-Ware` = coin::pvalue(coin::logrank_test(f, d, type = test))
  )
  pvalue <- round_small(x = res, method = "signif", digits = digits)
  if (is.numeric(pvalue)) {
    pvalsep <- " = "
    # the following line tries to figure out how to print p-values with specified digits
    # e.g. digits=2 -> "0.20" NOT "0.2"
    pvalue_f <- sprintf(paste0("%.", (max(digits, min(grep("[1-9]", strsplit(as.character(pvalue), "")[[1]]) - 3 + digits))), "f"), pvalue)
  } else {
    pvalsep <- " "
    pvalue_f <- pvalue
  }
  # pvaltxt <- paste("Log Rank p", pvalue, sep = pvalsep)
  # Bold p-value if requested and significant at sig.level
  parse_pval <- bold_pval & pvalue < sig.level
  if (parse_pval) {
    pvaltxt <- paste0(
      "paste(",
      paste0("\"", test, " p", "\","),
      paste0("\"", pvalsep, "\","),
      paste0("bold(\"", pvalue_f, "\")"),
      ")"
    )
  } else {
    pvaltxt <- paste(paste(test, "p"), pvalue_f, sep = pvalsep)
  }
  if (HR) {
    pretty.coxph.obj <- prettyCoxph(input.formula = f,
                                    input.d = d,
                                    ref.grp = cox.ref.grp,
                                    use.firth = use.firth)
    if (pretty.coxph.obj$used.firth) {
      coxm <- pretty.coxph.obj$fit.firth
    } else {
      coxm <- pretty.coxph.obj$fit
    }
    HRtxts <- Xunivcoxph(coxm, digits = digits)
    cox.strata.labs <- ystratalabs
    if (!is.null(cox.ref.grp)) {
      cox.strata.labs <- c(cox.ref.grp,
                           ystratalabs[ystratalabs != cox.ref.grp])
    }
    for (i in seq_along(HRtxts)) {
      HRtxt <- HRtxts[i]
      HRtxt <- paste0(HRtxt, " ~ ", cox.strata.labs[i + 1],
                      " vs. ", cox.strata.labs[1])
      p <- p + annotate(
        "text",
        x = 0.1 * max(fit$time),
        hjust = 0,
        y = lab.offset + line.y.increment * i,
        label = as_plotmath(HRtxt),
        size = size.summary,
        parse = TRUE
      )
    }
  }
  p <- p + annotate(
    "text",
    x = 0.1 * max(fit$time),
    hjust = 0,
    y = lab.offset,
    label = pvaltxt,
    size = size.summary,
    parse = parse_pval
  )
}
