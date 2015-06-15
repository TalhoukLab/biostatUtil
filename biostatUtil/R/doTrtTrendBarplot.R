#' Treatment trend barplots
#' @export
doTrtTrendBarplot <- function(input.d, title, legend.space = 0.2) {
  legend.space <- round(legend.space * nrow(input.d))
  treatment.trend <- c()
  treatment.trend.n <- c()
  temp.d <- input.d
  dx.years <- names(table(temp.d$dx.year))
  use.prop <- TRUE
  for (dx.year in dx.years) {
    select.cases <- temp.d$dx.year == dx.year
    n <- sum(!temp.d$init.treatment[select.cases] %in% ALL.MISSING.CODES)
    num.chemo.only <- sum(temp.d$init.treatment[select.cases] ==   "chemo.only")	
    num.rt.only    <- sum(temp.d$init.treatment[select.cases] ==   "rt.only")
    num.chemo.rt   <- sum(temp.d$init.treatment[select.cases] ==   "both")
    num.no.tx      <- sum(temp.d$init.treatment[select.cases] %in% c("no.treatment", "vag.brachy.only"))
    treatment <- c(num.chemo.only, num.rt.only, num.chemo.rt, num.no.tx)
    if (use.prop) {
      treatment <- treatment / n * 100
    }
    treatment.trend <- cbind(treatment.trend, treatment)
    treatment.trend.n <- c(treatment.trend.n, n)
  }
  colnames(treatment.trend) <- dx.years
  if (length(dx.years) > 20) {
    colnames(treatment.trend)[which(rep(c(FALSE, TRUE), 11))] <- ""
  }
  if (sum(dx.years == 2000) > 0) {
    colnames(treatment.trend)[dx.years == 2000] <- 2000
  }
  if (sum(dx.years == 1999) > 0) {
    colnames(treatment.trend)[dx.years == 1999] <- ""
  }
  if (sum(dx.years == 2001) > 0) {
    colnames(treatment.trend)[dx.years == 2001] <- ""
  }
  rownames(treatment.trend) <- c("chemo only", "rt only", "both", "no treatment or vag brachy only")
  
  # add dummy bar for legend
  treatment.trend <- cbind(treatment.trend, rep("", nrow(treatment.trend)))
  colnames(treatment.trend)[ncol(treatment.trend)] <- ""
  treatment.trend.n <- c(treatment.trend.n, legend.space)
  
  barplot(
    treatment.trend, width = treatment.trend.n, 
    legend.text = T, args.legend = list(x = "right"), col = c("red", "yellow", "brown", "blue"),
    las = 2,
    main = title,
    ylab = "Percentage of cases",
    xlab = "Year of diagnosis",
    sub = "(width of bar proportional to the number of patients)"
  )
}