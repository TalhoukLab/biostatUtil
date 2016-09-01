#' Treatment trend barplots
#' 
#' @param input.d input \code{data.frame}
#' @param title title of barplot
#' @param legend.space amount of space between legend and edge of plot
#' @param use.prop logical; if \code{TRUE}, proportions are used
#' @return a barplot showing the change of Treatment over time
#' @author Samuel Leung
#' @export
doTrtTrendBarplot <- function(input.d, title, legend.space = 0.2,
                              use.prop = TRUE) {
  legend.space <- round(legend.space * nrow(input.d))
  treatment.trend <- treatment.trend.n <- c()
  temp.d <- input.d
  dx.years <- names(table(temp.d$dx.year))
  for (dx.year in dx.years) {
    select.cases <- temp.d$dx.year == dx.year
    n <- sum(!temp.d$init.treatment[select.cases] %in% ALL.MISSING.CODES)
    num.chemo.only <- sum(temp.d$init.treatment[select.cases] ==
                            VALUE.CODING.INIT.TREATMENT.CHEMO.ONLY)	
    num.rt.only <- sum(temp.d$init.treatment[select.cases] ==
                         VALUE.CODING.INIT.TREATMENT.RT.ONLY)
    num.chemo.rt <- sum(temp.d$init.treatment[select.cases] ==
                          VALUE.CODING.INIT.TREATMENT.BOTH)
    num.no.tx <- sum(temp.d$init.treatment[select.cases] %in%
                       c(VALUE.CODING.INIT.TREATMENT.NO,
                         VALUE.CODING.INIT.TREATMENT.VAG.BRACHY.ONLY))
    treatment <- c(num.chemo.only, num.rt.only, num.chemo.rt, num.no.tx)
    if (use.prop)
      treatment <- treatment / n * 100
    treatment.trend <- cbind(treatment.trend, treatment)
    treatment.trend.n <- c(treatment.trend.n, n)
  }
  colnames(treatment.trend) <- dx.years
  if (length(dx.years) > 20)
    colnames(treatment.trend)[which(rep(c(FALSE, TRUE), 11))] <- ""
  if (sum(dx.years == 1999) > 0)
    colnames(treatment.trend)[dx.years == 1999] <- ""
  if (sum(dx.years == 2000) > 0)
    colnames(treatment.trend)[dx.years == 2000] <- 2000
  if (sum(dx.years == 2001) > 0)
    colnames(treatment.trend)[dx.years == 2001] <- ""
  rownames(treatment.trend) <- c("chemo only", "rt only", "both", "none")
  treatment.trend <- cbind(treatment.trend, rep("", nrow(treatment.trend)))
  colnames(treatment.trend)[ncol(treatment.trend)] <- ""
  treatment.trend.n <- c(treatment.trend.n, legend.space)
  barplot(treatment.trend, width = treatment.trend.n, legend.text = TRUE,
          args.legend = list(x = "right"),
          col = c("red", "yellow", "brown", "blue"), las = 2, main = title,
          ylab = "Percentage of cases", xlab = "Year of diagnosis",
          sub = "(width of bar proportional to the number of patients)")
}