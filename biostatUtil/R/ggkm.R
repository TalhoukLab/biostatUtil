#' Kaplan-Meier Plots using ggplot
#' 
#' Produce nicely annotated KM plots using ggplot.
#' 
#' @param sfit an object of class \code{survfit} containing one or more survival
#'   curves
#' @param sfit2 an (optional) second object of class \code{survfit} to compare
#'   with \code{sfit}
#' @param table logical; if \code{TRUE} (default), the numbers at risk at each
#'   time of death is shown as a table underneath the plot
#' @param returns logical; if \code{TRUE} the plot is returned
#' @param marks logical; if \code{TRUE} (default), censoring marks are shown on
#'   survival curves
#' @param CI logical; if \code{TRUE} (default), confidence bands are drawn for
#'   survival curves the using cumulative hazard, or log(survival).
#' @param line.pattern linetype for survival curves
#' @param shading.colors vector of colours for each survival curve
#' @param main plot title
#' @param xlabs horizontal axis label
#' @param ylabs vertical axis label
#' @param xlims horizontal limits for plot
#' @param ylims vertical limits for plot
#' @param ystratalabs labels for the strata being compared in \code{survfit}
#' @param cox.ref.grp indicates reference group for the variable of interest in
#'   the cox model.  this parameter will be ignored if not applicable, e.g. for 
#'   continuous variable
#' @param timeby length of time between consecutive time points spanning the
#'   entire range of follow-up. Defaults to 5.
#' @param pval logical; if \code{TRUE} (default), the logrank test p-value is
#'   shown on the plot
#' @param HR logical; if \code{TRUE} (default), the estimated hazard ratio and
#'   its 95\% confidence interval will be shown
#' @param use.firth Firth's method for Cox regression is used if the percentage
#'   of censored cases exceeds \code{use.firth}. Setting \code{use.firth = 1}
#'   (default) means Firth is never used, and \code{use.firth = -1} means Firth
#'   is always used.
#' @param subs use of subsetting
#' @param legend logical; if \code{TRUE}, the legend is overlaid on the graph
#'   (instead of on the side).
#' @param legend.xy named vector specifying the x/y position of the legend
#' @param legend.direction layout of items in legends ("horizontal" (default) or
#'   "vertical")
#' @param line.y.increment how much y should be incremented for each line
#' @param digits number of digits to round: p-values digits=nunber of
#'   significant digits, HR digits=number of digits after decimal point NOT
#'   significant digits
#' @param min.p.value the min. p-value below which the p-value will be shown as
#'   e.g. <0.0001, otherwise the exact p-value will be shown
#' @param ... additional arguments to other methods
#' @export
ggkm <- function(sfit, sfit2 = NULL, table = TRUE, returns = TRUE,
                 marks = TRUE, CI = TRUE,
                 line.pattern = NULL,
                 shading.colors = c("blue2", "red2",
                                    "deepskyblue", "indianred3"),
                 main = "Kaplan-Meier Plot",
                 xlabs = "Time", ylabs = "Survival Probability",
                 xlims = c(0, max(sfit$time)), ylims = c(0, 1),
                 ystratalabs = NULL, cox.ref.grp = NULL,
                 timeby = 5, pval = TRUE, HR = TRUE,
                 use.firth = 1, subs = NULL, 
                 legend = FALSE, legend.xy = c("x" = 0.8, "y" = 0.88),
                 legend.direction = "horizontal",
                 line.y.increment = 0.05, digits = 3, min.p.value = 0.0001,
                 ...) {
  time <- surv <- lower <- upper <- n.censor <- n.risk <- NULL
  times <- seq.int(0, max(sfit$time), by = timeby)
  s1 <- levels(summary(sfit)$strata)
  s2 <- summary(sfit, censored = TRUE)$strata
  s3 <- summary(sfit, times = times, extend = TRUE)$strata
  if (is.null(subs)) {
    subs1 <- seq_along(s1)
    subs2 <- seq_along(s2)
    subs3 <- seq_along(s3)
  } else {
    for (i in seq_along(subs)) {
      if (i == 1)
        ssvar <- paste0("(?=.*\\b=", subs[i])
      if (i == length(subs))
        ssvar <- paste0(ssvar, "\\b)(?=.*\\b=", subs[i], "\\b)")	
      if (!i %in% c(1, length(subs)))
        ssvar <- paste0(ssvar, "\\b)(?=.*\\b=", subs[i])	
      if (i == 1 & i == length(subs))
        ssvar <- paste0("(?=.*\\b=", subs[i], "\\b)")
    }
    subs1 <- which(regexpr(ssvar, s1, perl = TRUE) != -1)
    subs2 <- which(regexpr(ssvar, s2, perl = TRUE) != -1)
    subs3 <- which(regexpr(ssvar, s3, perl = TRUE) != -1)
  }
  if (!is.null(cox.ref.grp))
    names(cox.ref.grp) <- all.vars(sfit$call)[3]  # works for two-sided formulas
  if (!is.null(subs))
    pval <- FALSE
  if (is.null(ystratalabs))
    ystratalabs <- gsub(".*=(.)", "\\1", names(sfit$strata))
  mleft <- left_margin(ystratalabs)  # left margins for km plot and risk table
  
  .df <- sfit %>%   # data to be used in the survival plot
    broom::tidy() %>% 
    select(time, n.risk, n.event, n.censor, surv = estimate,
           strata, upper = conf.high, lower = conf.low) %>% 
    mutate(strata = factor(s2, labels = ystratalabs))
  zeros <- data.frame(time = 0, surv = 1,
                      strata = factor(ystratalabs, levels = levels(.df$strata)),
                      upper = 1, lower = 1)
  .df <- bind_rows(zeros, .df)
  d <- length(levels(.df$strata))
  
  # Specifying plot parameteres etc
  names(shading.colors) <- ystratalabs
  if (is.null(line.pattern) | length(line.pattern) == 1)
    line.pattern <- setNames(rep(1, d), ystratalabs)
  p <- ggplot(.df , aes(time, surv, color = strata, fill = strata,
                        linetype = strata)) +
    geom_step(size = .7) + 
    scale_colour_manual(values = shading.colors) +
    scale_fill_manual(values = shading.colors) + 
    scale_linetype_manual(values = line.pattern) +
    scale_x_continuous(xlabs, breaks = times, limits = xlims) + 
    scale_y_continuous(ylabs, limits = ylims) + 
    theme_bw() +
    theme(axis.title.x = element_text(vjust = 0.5),
          panel.background = element_blank(),
          panel.grid = element_blank(),
          plot.margin = grid::unit(c(0, 1, .5, mleft$margin.km),
                                   c("lines", "lines", "lines", "in"),
                                   list(NULL, NULL, NULL, NULL))) +
    ggtitle(main)
  if (legend == TRUE)  # Legend
  p <- p + theme(legend.position = legend.xy[c("x", "y")]) +
    theme(legend.key = element_rect(colour = NA)) +
    theme(legend.title = element_blank()) +
    theme(legend.direction = "horizontal")
  else
    p <- p + theme(legend.position = "none")
  if (CI == TRUE)  # Confidence Bands
  p <- p + geom_ribbon(data = .df, aes(ymin = lower, ymax = upper),
                       alpha = 0.05, linetype = 0) 
  if (marks == TRUE)  # Censor Marks
  p <- p + geom_point(data = subset(.df, n.censor >= 1), 
                      aes(x = time, y = surv), shape = "/", size = 4)
  # Statistics placement
  if (is.null(sfit2))
    fit <- sfit
  else
    fit <- sfit2
  p <- summarize_km(fit = fit, p = p, pval = pval, min.p.value = min.p.value,
                    digits = digits, HR = HR, cox.ref.grp = cox.ref.grp,
                    use.firth = use.firth, ystratalabs = ystratalabs,
                    line.y.increment = line.y.increment)
  
  # Create table graphic to include at-risk numbers, keep at-risk numbers 
  # same order as appears in HR (do not reverse levels)
  if (table) {
    risk.data <- sfit %>% 
      summary(times = times, extend = TRUE) %>% 
      list() %>% 
      purrr::map_df(`[`, c("strata", "time", "n.risk"))
    data.table <- ggplot(risk.data, aes(x = time, y = strata,
                                        label = format(n.risk, nsmall = 0))) +
      geom_text(size = 3.5) +
      scale_y_discrete(labels = ystratalabs) +
      scale_x_continuous("Numbers at risk", limits = xlims) +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_blank(),
            axis.ticks = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_text(color = shading.colors[ystratalabs],
                                       face = "bold", hjust = 1),
            axis.title.x = element_text(size = 12, vjust = 1),
            legend.position = "none",
            plot.margin = grid::unit(c(0.5, 1.25, 0.5, mleft$margin.rt),
                                     "lines")) +
      labs(y = NULL)
    if (returns)
      gridExtra::grid.arrange(p, data.table, clip = FALSE, nrow = 2, ncol = 1,
                              heights = grid::unit(c(2, .5), "null"))
   } else {
    return(p)
  }
}

#' Get left margin distances for km plot and risk table
#' @noRd
left_margin <- function(labels) {
  max.nc <- max(nchar(labels))
  if (max.nc <= 4) {
    mleft.km <- 0
    mleft.rt <- 2.4
  } else {
    mleft.km <- graphics::strwidth(labels[which.max(nchar(labels))],
                                   units = "in")
    if (max.nc <= 5) {
      mleft.km <- mleft.km - (max.nc / 100 + 0.4)
    } else {
      mleft.km <- mleft.km - (max.nc / 100 + 0.5)
    }
    mleft.rt <- 0.5
  }
  return(list(margin.km = mleft.km,
              margin.rt = mleft.rt))
}

#' Numerical summaries of km fit: HR (95\% CI), Log rank test p-value
#' @noRd
summarize_km <- function(fit, p, pval, min.p.value, digits, HR, cox.ref.grp,
                         use.firth, ystratalabs, line.y.increment) {
  if (pval) {
    sdiff <- survdiff(eval(fit$call$formula), data = eval(fit$call$data))
    pval <- pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
    pvaltxt <- ifelse(pval < min.p.value,
                      paste("Log Rank p <", format(min.p.value,
                                                   scientific = FALSE)),
                      paste("Log Rank p =", signif(pval, digits)))
    if (HR) {
      pretty.coxph.obj <- prettyCoxph(eval(fit$call$formula),
                                      input.d = eval(fit$call$data),
                                      ref.grp = cox.ref.grp,
                                      use.firth = use.firth)
      if (pretty.coxph.obj$used.firth) {
        coxm <- pretty.coxph.obj$fit.firth
      } else {
        coxm <- pretty.coxph.obj$fit
      }
      HRtxts <- Xunivcoxph(coxm, digits = digits)
      show.ref.group <- length(HRtxts) > 1
      cox.strata.labs <- ystratalabs
      if (!is.null(cox.ref.grp)) {
        cox.strata.labs <- c(cox.ref.grp,
                             ystratalabs[ystratalabs != cox.ref.grp])
      }
      for (i in seq_along(HRtxts)) {
        HRtxt <- HRtxts[i]
        if (show.ref.group) {
          HRtxt <- paste0(HRtxt, " ~ ", cox.strata.labs[i + 1],
                          " vs. ", cox.strata.labs[1])
        }
        p <- p + annotate("text", x = 0.2 * max(fit$time), hjust = 0,
                          y = 0.01 + line.y.increment * i, label = HRtxt,
                          size = 3)					
      }
    }
  }
  p <- p + annotate("text", x = 0.2 * max(fit$time), hjust = 0, y = 0.01,
                    label = ifelse(pval, pvaltxt, ""), size = 3)
}