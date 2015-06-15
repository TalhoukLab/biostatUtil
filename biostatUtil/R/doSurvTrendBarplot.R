#' Survival trend barplot
#' @export
doSurvTrendBarplot <- function(input.d, title, surv.yrs, surv.sts,
                                      surv.event, event.desc, censor.desc,
                                      obs.yrs = 3, legend.space = 0.2) {
  legend.space <- round(legend.space * nrow(input.d))
  survival.trend <- c()
  survival.trend.n <- c()
  temp.d <- input.d
  dx.years <- names(table(temp.d$dx.year))
  use.prop <- TRUE
  for (dx.year in dx.years) {
    select.cases <- temp.d$dx.year == dx.year
    n <- sum(!is.na(temp.d[select.cases, surv.yrs]))
    event <- sum(temp.d[select.cases, surv.sts] == surv.event &
                   as.numeric(temp.d[select.cases, surv.yrs]) <= obs.yrs,
                 na.rm = TRUE)
    censor <- n - event
    
    survival <- c(event, censor)
    if (use.prop) {
      survival <- survival / n * 100
    }
    survival.trend <- cbind(survival.trend, survival)
    survival.trend.n <- c(survival.trend.n, n)
  }
  colnames(survival.trend) <- dx.years
  if (length(dx.years) > 20) {
    colnames(survival.trend)[which(rep(c(FALSE, TRUE), 11))] <- ""
  }
  if (sum(dx.years == 2000) > 0) {
    colnames(survival.trend)[dx.years == 2000] <- 2000
  }
  if (sum(dx.years == 1999) > 0) {
    colnames(survival.trend)[dx.years == 1999] <- ""
  }
  if (sum(dx.years == 2001) > 0) {
    colnames(survival.trend)[dx.years == 2001] <- ""
  }
  rownames(survival.trend) <- c(event.desc, censor.desc)
  
  # add dummy bar for legend
  survival.trend <- cbind(survival.trend, rep("", nrow(survival.trend)))
  colnames(survival.trend)[ncol(survival.trend)] <- ""
  survival.trend.n <- c(survival.trend.n, legend.space)
  
  barplot(
    survival.trend, width = survival.trend.n, 
    legend.text = T, args.legend = list(x = "right"), col = c("grey", "pink"),
    las = 2,
    main = title,
    ylab = "Percentage of cases",
    xlab = "Year of diagnosis",
    sub = "(width of bar proportional to the number of patients)"
  )
}