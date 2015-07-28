#' Plot Kaplan-Meier curves
#' 
#' KM plots with basic annotations.
#' 
#' @param input.d input \code{data.frame}
#' @param input.formula survival formula to \code{Surv}
#' @param main.text plot title
#' @param xlab.text x-axis label
#' @param ylab.text y-axis label
#' @param line.name name of legend
#' @param line.color line colour of survival curves
#' @param line.pattern line pattern of survival curves
#' @param line.width line width of survival curves
#' @param show.test show single or the reference group value (for pairwise comparisons).
#' If \code{"none"}, then no test is show.
#' @param single.test.type test to show if specified \code{show.test = "single"}. Possible
#' choices are \code{"logrank"} (default), \code{"wilcoxon"}, \code{"taroneware"}, or \code{"all"}.
#' @param round.digits.p.value number of digits for p-value
#' @param obs.survyrs show the observed survival years survival rate on KM plot
#' @param legend.pos legend position keyword
#' @param file.name name of file to save plot to
#' @param file.width width of figure in saved file
#' @param file.height height of figure in saved file
#' @param grey.scale logical. If \code{TRUE}, the plot will be in grey scale.
#' @param show.single.test.pos position to show single test; defaults to 0.5 if
#' \code{legend.pos = "top"}. Otherwise 0.1
#' @param ... additional arguments to \code{plotKMDetail}
#' @author Samuel Leung
#' @seealso \code{\link{plotKMDetail}}
#' @export
plotKM <- function(input.d, input.formula, main.text, xlab.text, ylab.text,
                   line.name, line.color, line.pattern = NULL,
                   line.width = NULL, show.test = "single",
                   single.test.type = "logrank", round.digits.p.value = 4,
                   obs.survyrs = 10, legend.pos = "bottomleft",
                   file.name = "no.file", file.width = 7, file.height = 7,
                   grey.scale = FALSE, show.single.test.pos = "default", ...) {
  
  # calculate "obs.survyrs"-yrs survival
  summary.surv.fit <- summary(survfit(input.formula, data = input.d),
                              time = obs.survyrs, extend = T)
  
  n.cases <- table(input.d[, deparse(input.formula[[3]])]) # number of cases in each group variable = deparse(input.formula[[3]])
  decrement.count <- 0 # the number we need to take away from "i" because if n.cases[i]==0, length(summary.surv.fit) < length(line.name)
  
  ten.yrs.surv <- NULL
  for (i in 1:length(line.name)) {
    if (n.cases[i] == 0) {
      # there must be no cases in this category
      ten.yrs.surv <- c(ten.yrs.surv, "NA")
      decrement.count <- decrement.count + 1
    } else {
      ten.yrs.surv <- c(ten.yrs.surv,
                        paste0(format(summary.surv.fit$surv * 100,
                                      digits = 3)[i - decrement.count], "% (",
                              format(summary.surv.fit$lower * 100,
                                     digits = 3)[i - decrement.count], "% - ",
                              format(summary.surv.fit$upper * 100,
                                     digits = 3)[i - decrement.count], "%)"))
    }
  }                   
  
  # summary of survival object to end of followup
  fit.obj <- survfit(input.formula, data = input.d)		
  summary.surv.fit.all <- summary(fit.obj)[['table']]
  
  # NEED TO RESET decrement.count!!!!
  decrement.count <- 0 # the number we need to take away from "i" because if n.cases[i]==0, length(summary.surv.fit) < length(line.name)
  
  event.count <- NULL
  for (i in 1:length(line.name)) {
    if (n.cases[i] == 0) {
      # there must be no cases in this category
      event.count <- c(event.count, "NA")
      decrement.count <- decrement.count + 1
    } else {
      event.count <- c(event.count,
                       paste0(summary.surv.fit.all[i - decrement.count,
                                                  'events'], "/",
                             summary.surv.fit.all[i - decrement.count,
                                                  'records']))
    }
  }
  
  # determine show.single.test.pos
  if (show.single.test.pos == "default") {
    show.single.test.pos <- 0.1
    if (legend.pos == "top") {
      show.single.test.pos <- 0.5
    }
  }
  # plot km
  output <- plotKMDetail(input.d,
                         input.formula,
                         main.text, 
                         xlab.text, 
                         ylab.text,
                         line.name,
                         ten.yrs.surv,
                         event.count,
                         line.color,
                         obs.survyrs,
                         line.pattern = line.pattern,
                         line.width = line.width,
                         legend.pos = legend.pos,
                         file.name = file.name,
                         file.width = file.width,
                         file.height = file.height,
                         show.test = show.test,
                         round.digits.p.value = round.digits.p.value,
                         single.test.type = single.test.type,
                         grey.scale = grey.scale,
                         show.single.test.pos = show.single.test.pos, ...)
  return(list("log.rank.p.values" = output$log.rank.p.values,
              "wilcox.p.values" = output$wilcox.p.values,
              "n" = sum(fit.obj$n),
              "nevent" = sum(fit.obj$n.event)))
}