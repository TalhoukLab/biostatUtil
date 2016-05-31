#' Kaplan-Meier Plots using ggplot
#' 
#' Produce nicely annotated KM plots using ggplot.
#' 
#' @param sfit an object of class \code{survfit} containing one or more survival curves
#' @param sfit2 an (optional) second object of class \code{survfit} to compare with \code{sfit}
#' @param table logical; if \code{TRUE} (default), the numbers at risk at each time of death is
#' shown as a table underneath the plot
#' @param returns logical; if \code{TRUE} the plot is returned
#' @param marks logical; if \code{TRUE} (default), censoring marks are shown on survival curves
#' @param CI logical; if \code{TRUE} (default), confidence bands are drawn for survival curves
#' @param shading.colors vector of colours for each survival curve
#' @param main plot title
#' @param xlabs horizontal axis label
#' @param ylabs vertical axis label
#' @param xlims horizontal limits for plot
#' @param ylims vertical limits for plot
#' @param ystratalabs labels for the strata being compared in \code{survfit}
#' @param ystrataname name of the strata
#' @param cox.ref.grp indicates reference group for the variable of interest 
#' in the cox model.  this parameter will be ignored if not applicable, e.g. for 
#' continuous variable
#' @param timeby length of time between consecutive time points spanning the entire range of follow-up. Defaults to 5.
#' @param pval logical; if \code{TRUE} (default), the logrank test p-value is shown on the plot
#' @param HR logical; if \code{TRUE} (default), the estimated hazard ratio and its 95\% confidence interval will be shown
#' @param use.firth Firth's method for Cox regression is used if the percentage of censored cases exceeds \code{use.firth}.
#' Setting \code{use.firth = 1} (default) means Firth is never used, and \code{use.firth = -1} means Firth is always used.
#' @param subs use of subsetting
#' @param legend logical; if \code{TRUE}, the legend is overlaid on the graph (instead of on the side).
#' @param line.y.increment how much y should be incremented for each line
#' @param ... additional arguments to other methods
#' @import ggplot2
#' @export
ggkm <- function(sfit, sfit2 = NULL, table = TRUE, returns = TRUE,
                 marks = TRUE, CI = TRUE,
                 shading.colors = c("blue2", "red2",
                                    "deepskyblue", "indianred3"),
                 main = "Kaplan-Meier Plot",
                 xlabs = "Time", ylabs = "Survival Probability",
                 xlims = c(0, max(sfit$time)), ylims = c(0, 1),
                 ystratalabs = NULL, ystrataname = NULL, cox.ref.grp = NULL,
                 timeby = 5, pval = TRUE, HR = TRUE,
                 use.firth = 1, subs = NULL, legend = FALSE,
                 line.y.increment = 0.05, ...) {
  time <- surv <- lower <- upper <- n.censor <- n.risk <- NULL
  times <- seq(0, max(sfit$time), by = timeby)
  if (is.null(subs)) {
    subs1 <- 1:length(levels(summary(sfit)$strata))
    subs2 <- 1:length(summary(sfit, censored = TRUE)$strata)
    subs3 <- 1:length(summary(sfit, times = times, extend = TRUE)$strata)
  } else {
    for (i in 1:length(subs)) {
      if (i == 1)
        ssvar <- paste0("(?=.*\\b=", subs[i])
      if (i == length(subs))
        ssvar <- paste0(ssvar, "\\b)(?=.*\\b=", subs[i], "\\b)")	
      if (!i %in% c(1, length(subs)))
        ssvar <- paste0(ssvar, "\\b)(?=.*\\b=", subs[i])	
      if (i == 1 & i == length(subs))
        ssvar <- paste0("(?=.*\\b=", subs[i], "\\b)")
    }
    subs1 <- which(regexpr(ssvar, levels(summary(sfit)$strata),
                           perl = T) != -1)
    subs2 <- which(regexpr(ssvar, summary(sfit, censored = TRUE)$strata,
                           perl = T) != -1)
    subs3 <- which(regexpr(ssvar, summary(sfit, times = times,
                                          extend = TRUE)$strata,
                           perl = T) != -1)
  }
  if (!is.null(subs))
    pval <- FALSE
  if (is.null(ystratalabs))
    ystratalabs <- as.character(sub("group=*", "", names(sfit$strata))) 
  if (is.null(ystrataname))
    ystrataname <- "Strata"
  m <- max(nchar(ystratalabs))
  times <- seq(0, max(sfit$time), by = timeby)
  if (!is.null(cox.ref.grp)) {
	  # very ugly ... hope this will work for most cases!
	  names(cox.ref.grp) <- strsplit(names(sfit$strata)[1],"=")[[1]][1]
  }
  
  .df <- data.frame(                      # data to be used in the survival plot
    time = sfit$time[subs2],
    n.risk = sfit$n.risk[subs2],
    n.event = sfit$n.event[subs2],
    n.censor = sfit$n.censor[subs2],
    surv = sfit$surv[subs2],
    strata = factor(summary(sfit, censored = TRUE)$strata[subs2]),
    upper = sfit$upper[subs2],
    lower = sfit$lower[subs2])
  levels(.df$strata) <- ystratalabs       # final changes to data for survival plot
  zeros <- data.frame(
    time = 0, surv = 1,
    strata = factor(ystratalabs, levels = levels(.df$strata)),
    upper = 1, lower = 1)
  .df <- plyr::rbind.fill(zeros, .df)
  d <- length(levels(.df$strata))
  
  # specifying plot parameteres etc ----
  names(shading.colors) <- ystratalabs
  colScale <- scale_colour_manual(values = shading.colors)
  colFill <- scale_fill_manual(values = shading.colors)
  
  p <- ggplot(.df , aes(time, surv, color = strata, fill = strata)) +
    geom_step(aes(color = strata), size = .7) + 
    theme_bw() +
    colScale+
    colFill+ 
    theme(axis.title.x = element_text(vjust = 0.5)) + 
    scale_x_continuous(xlabs, breaks = times, limits = xlims) + 
    scale_y_continuous(ylabs, limits = ylims) + 
    theme(panel.background = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    theme(plot.margin = grid::unit(c(0, 1, .5,
                                     ifelse(m < 10, 1.5, 2.5)), "lines")) +
    ggtitle(main)
  if (legend == TRUE)  # Legend----
    p <- p + theme(legend.position = c(.8, .88)) +
      theme(legend.key = element_rect(colour = NA)) +
      theme(legend.title = element_blank())
  else
    p <- p + theme(legend.position = "none")
  if (CI == TRUE)  # Confidence Bands----  
    p <- p + geom_ribbon(data = .df, aes(ymin = lower, ymax = upper),
                         alpha = 0.05, linetype = 0) 
  if (marks == TRUE)  # Censor Marks----
    p <- p + geom_point(data = subset(.df, n.censor >= 1), 
                        aes(x = time, y = surv), shape = "/", size = 4)
  ## Create a blank plot for place-holding
  blank.pic <- ggplot(.df, aes(time, surv)) + geom_blank() + theme_bw() +
    theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
          axis.title.x = element_blank(), axis.title.y = element_blank(),
          axis.ticks = element_blank(), panel.grid.major = element_blank(),
          panel.border = element_blank())
  # p-value placement----
  if (is.null(sfit2)) {
    if (pval) {
      sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
      pval <- pchisq(sdiff$chisq,length(sdiff$n) - 1, lower.tail = FALSE)
      pvaltxt <- ifelse(pval < 0.001, "Log Rank p < 0.001",
                        paste("Log Rank p =", signif(pval, 3)))
      if (HR) {
        pretty.coxph.obj <- prettyCoxph(eval(sfit$call$formula),
                                        input.d = eval(sfit$call$data),
										ref.grp = cox.ref.grp,
                                        use.firth = use.firth)
        if (pretty.coxph.obj$used.firth) {
          coxm <- pretty.coxph.obj$fit.firth
          HRtxts <- Xunivcoxph(coxm, coxph.type = "coxphf")
        } else {
          coxm <- pretty.coxph.obj$fit
          HRtxts <- Xunivcoxph(coxm)
        }
        show.ref.group <- length(HRtxts) > 1
		cox.strata.labs <- ystratalabs
		if (!is.null(cox.ref.grp)) {
			cox.strata.labs <- c(cox.ref.grp,ystratalabs[ystratalabs!=cox.ref.grp])
		}
        for (i in 1:length(HRtxts)) {
          HRtxt <- HRtxts[i]
          if (show.ref.group) {
            HRtxt <- paste0(HRtxt, " ~ ", cox.strata.labs[i + 1],
                            " vs. ", cox.strata.labs[1])
          }
          p <- p + annotate("text", x = 0.2 * max(sfit$time), hjust = 0,
                            y = 0.01 + line.y.increment * i, label = HRtxt,
                            size = 3)					
        }
      }
    }
    p <- p + annotate("text", x = 0.2 * max(sfit$time), hjust = 0, y = 0.01,
                      label = ifelse(pval, pvaltxt, ""), size = 3)
  } else {
    if (pval) {
      sdiff <- survdiff(eval(sfit2$call$formula), data = eval(sfit2$call$data))
      pval <- pchisq(sdiff$chisq, length(sdiff$n) - 1, lower.tail = FALSE)
      pvaltxt <- ifelse(pval < 0.001, "Log Rank p < 0.001",
                        paste("Log Rank p =", signif(pval, 3)))
      if (HR) {
        pretty.coxph.obj <- prettyCoxph(eval(sfit2$call$formula),
                                        input.d = eval(sfit2$call$data),
										ref.grp = cox.ref.grp,
                                        use.firth = use.firth)
        if (pretty.coxph.obj$used.firth) {
          coxm <- pretty.coxph.obj$fit.firth
          HRtxts <- Xunivcoxph(coxm, coxph.type = "coxphf")
        } else {
          coxm <- pretty.coxph.obj$fit
          HRtxts <- Xunivcoxph(coxm)
        }
        show.ref.group <- length(HRtxts) > 1
		cox.strata.labs <- ystratalabs
		if (!is.null(cox.ref.grp)) {
			cox.strata.labs <- c(cox.ref.grp,ystratalabs[ystratalabs!=cox.ref.grp])
		}
        for (i in 1:length(HRtxts)) {
          HRtxt <- HRtxts[i]
          if (show.ref.group) {
            HRtxt <- paste0(HRtxt, " ~ ", cox.strata.labs[i + 1],
                           " vs. ", cox.strata.labs[1])
          }
          p <- p + annotate("text", x = 0.2 * max(sfit2$time), hjust = 0,
                            y = 0.01 + line.y.increment * i, label = HRtxt,
                            size = 3)					
        }
      }
    }
    p <- p + annotate("text", x = 0.2 * max(sfit2$time), y = 0.01,
                      label = ifelse(pval, pvaltxt, ""), size = 3)
  }
  
  # Create table graphic to include at-risk numbers----
  if (table) {
    risk.data <- data.frame(
      strata = factor(summary(sfit,times = times,
                              extend = TRUE)$strata[subs3], ),
      time = as.numeric(summary(sfit,times = times,
                                extend = TRUE)$time[subs3]) * 0.97 +
        xlims[2] * 0.02, # doing some nasty empirical scaling/shifting here!!!  remove scaling/shifting factors if dosen't look right
      n.risk = summary(sfit, times = times, extend = TRUE)$n.risk[subs3]
    )
    risk.data$strata <- factor(risk.data$strata,
			# want to print at-risk numbers same order as appears in HR, 
			# therefore, do not rev the levels
			levels = levels(risk.data$strata))#levels = rev(levels(risk.data$strata)))
    data.table <- ggplot(risk.data, aes(x = time, y = strata,
                                        label = format(n.risk, nsmall = 0))) +
      geom_text(size = 3.5) + theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
			  # want to print at-risk numbers same order as appears in HR, 
			  # therefore, do not rev the levels		  
			  labels = ystratalabs) +#labels = rev(ystratalabs)) +
      scale_x_continuous("Numbers at risk", limits = xlims) +
      theme(axis.title.x = element_text(size = 12, vjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_text(face = "bold",
				# want to print at-risk numbers same order as appears in HR, 
				# therefore, do not rev the levels					
				color = shading.colors[1:length(ystratalabs)],#color = rev(shading.colors[1:length(ystratalabs)]),
				hjust = 1),
            legend.position = "none",
            plot.margin = grid::unit(
              c(-1.5, 1, 0.1, ifelse(m < 10, 2.5, 3.5) - 0.28 * m), "lines")) +
      xlab(NULL) + ylab(NULL)
    if (returns)
      gridExtra::grid.arrange(p, blank.pic, data.table,
                              clip = FALSE, nrow = 3, ncol = 1,
                              heights = grid::unit(c(2, .1, .25),
                                                   c("null", "null", "null")))
  } else {
    return(p)
  }
}
