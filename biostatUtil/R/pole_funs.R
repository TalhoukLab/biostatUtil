#' Testing for interaction in survival models
#' @param dmat data matrix
#' @param event survival outcome. Must be one of "OS", "DSS", or "RFS".
#' @param labs labels for each of the 3 models to test
#' @param use.firth if \code{TRUE}, will use Firth's corrected Cox regression
#' model.
#' @return A summary output for each model showing test for interaction
#' @author Aline Talhouk
#' @export
testInter <- function(dmat, event, labs = c("POLE wt", "Rx", "POLE wt * Rx"),
                      use.firth = FALSE) {
  dmat$POLE <- as.numeric(dmat$POLE) - 1
  dmat$Rx <- as.numeric(dmat$Rx) - 1
  if (use.firth) {
    switch(event,
           OS = {
             cox1 = coxphf::coxphf(Surv(OS.yrs, OS.sts == "os.event") ~
                                     POLE, data = dmat)
             cox2 = coxphf::coxphf(Surv(OS.yrs, OS.sts == "os.event") ~
                                     POLE + Rx, data = dmat)
             cox3 = coxphf::coxphf(Surv(OS.yrs, OS.sts == "os.event") ~
                                     POLE * Rx, data = dmat)
           },
           DSS = {
             cox1 = coxphf::coxphf(Surv(DSS.yrs, DSS.sts == "dss.event") ~
                                     POLE, data = dmat)         
             cox2 = coxphf::coxphf(Surv(DSS.yrs, DSS.sts == "dss.event") ~
                                     POLE + Rx, data = dmat)
             cox3 = coxphf::coxphf(Surv(DSS.yrs, DSS.sts == "dss.event") ~
                                     POLE * Rx, data = dmat)  		
           },
           RFS = {
             cox1 = coxphf::coxphf(Surv(RFS.yrs, RFS.sts == "rfs.event") ~
                                     POLE, data = dmat)
             cox2 = coxphf::coxphf(Surv(RFS.yrs, RFS.sts == "rfs.event") ~
                                     POLE + Rx, data = dmat)
             cox3 = coxphf::coxphf(Surv(RFS.yrs, RFS.sts == "rfs.event") ~
                                     POLE * Rx, data = dmat)         
           }
    )
  } else {
    switch(event,
           OS = {
             cox1 = coxph(Surv(OS.yrs, OS.sts == "os.event") ~
                            POLE, data = dmat)
             cox2 = coxph(Surv(OS.yrs, OS.sts == "os.event") ~
                            POLE + Rx, data = dmat)
             cox3 = coxph(Surv(OS.yrs, OS.sts == "os.event") ~
                            POLE * Rx, data = dmat)
           },
           DSS = {
             cox1 = coxph(Surv(DSS.yrs, DSS.sts == "dss.event") ~
                            POLE, data = dmat)         
             cox2 = coxph(Surv(DSS.yrs, DSS.sts == "dss.event") ~
                            POLE + Rx, data = dmat)
             cox3 = coxph(Surv(DSS.yrs, DSS.sts == "dss.event") ~
                            POLE * Rx, data = dmat)  
           },
           RFS = {
             cox1 = coxph(Surv(RFS.yrs, RFS.sts == "rfs.event") ~
                            POLE, data = dmat)
             cox2 = coxph(Surv(RFS.yrs, RFS.sts == "rfs.event") ~
                            POLE + Rx, data = dmat)
             cox3 = coxph(Surv(RFS.yrs, RFS.sts == "rfs.event") ~
                            POLE * Rx, data = dmat)         
           }
    )
  }
  mod1 <- coxphOut(cox1, coefnames = labs[1])
  mod2 <- coxphOut(cox2, coefnames = labs[1:2])
  mod3 <- coxphOut(cox3, coefnames = labs)
  if (use.firth) {
    lrt1.p <- coxphf::coxphftest(cox2$formula, test = ~POLE,
                                 data = dmat)$prob
    lrt2.p <- coxphf::coxphftest(cox3$formula, test = ~POLE + Rx,
                                 data = dmat)$prob
  } else {
    lrt1.p <- anova(cox2, cox1)["P(>|Chi|)"][2, ]
    lrt2.p <- anova(cox3, cox2)["P(>|Chi|)"][2, ]
  }
  return(list("mod1" = mod1, "mod2" = mod2, "mod3" = mod3,
              "lrt1.p" = lrt1.p, "lrt2.p" = lrt2.p))
}

#' Print intereraction test results for different models
#' @param mod1 model 1
#' @param mod2 model 2
#' @param mod3 model 3
#' @param Capt caption for the table
#' @return an HTML formated table showing different models tested for
#'   interactions
#' @author Aline Talhouk
#' @export
printInterModels <- function(mod1, mod2, mod3, Capt) {
  vars <- rbind(mod1, mod2, mod3)
  cgroup <- c("Coefficients", "HR and [95% CI]")
  n.cgroup <- c(4, 3)
  rgroup <- c("Model 1", "Model 2", "Model 3")
  n.rgroup <- c(nrow(mod1), nrow(mod2), nrow(mod3))
  TAB <- htmlTable::htmlTable(vars, 
                              rowlabel = "Models Considered", 
                              rgroup = rgroup,
                              n.rgroup = n.rgroup, 
                              caption = paste0(Capt, "Models <sup>&dagger;</sup>
                                               testing for interactions"), 
                              tfoot = "<sup>&dagger;</sup> Model 1 includes POLE only,
                              Model 2 includes POLE adjusted for treatment,
                              and Model 3 includes POLE, treatment and an interaction",
                              ctable = TRUE)
  pander::pander(TAB, style = 'rmarkdown')
}

#' Make Kaplan-Meier plots for POLE
#' 
#' @param data input data for \code{doKMPlots}.
#' @param surv.type survival outcome. Either "os", "dss", or "rfs".
#' @param use.aline.plot If \code{TRUE}, use aline's plot function
#' @param ... additional arguments to \code{doKMPlots}
#' @return Kaplan-Meier plots plotted for POLE mutations
#' @author Samuel Leung
#' @export
doKMPlotsPOLE <- function(data, surv.type, use.aline.plot = FALSE, ...) {
  doKMPlots(
    data[!data$POLE.mut.germline.as.missing %in% ALL.MISSING.CODES, ],
    var.name = "POLE.mut.germline.as.missing", var.description = "POLE mutation status - whole cohort",
    single.test.type = "logrank", surv.type = surv.type,
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
  
  doKMPlots(
    data[!data$POLE.mut.germline.as.missing %in% ALL.MISSING.CODES &
           !data$init.treatment %in% ALL.MISSING.CODES &
           data$init.treatment == VALUE.CODING.INIT.TREATMENT.NO, ],
    var.name = "POLE.mut.germline.as.missing",
    var.description = "POLE mutation status - no adjuvant treatment",
    single.test.type = "logrank", surv.type = surv.type,
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
  
  doKMPlots(
    data[!data$POLE.mut.germline.as.missing %in% ALL.MISSING.CODES &
           !data$init.treatment %in% ALL.MISSING.CODES &
           data$init.treatment != VALUE.CODING.INIT.TREATMENT.NO, ],
    var.name = "POLE.mut.germline.as.missing",
    var.description = "POLE mutation status - any adjuvant treatment",
    single.test.type = "logrank", surv.type = surv.type,
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
}

#' Make Kaplan-Meier plots with initial treatment
#' 
#' @param data input data for \code{doKMPlots}.
#' @param surv.type survival outcome. Either "os", "dss", or "rfs".
#' @param use.aline.plot If \code{TRUE}, use aline's plot function
#' @param ... additional arguments to \code{doKMPlots}
#' @return Kaplan-Meier plots plotted for POLE mutations
#' @author Samuel Leung
#' @export
doKMPlotsPOLEInitTrt <- function(data, surv.type,
                                 use.aline.plot = FALSE, ...) {
  doKMPlots(
    data[!data$POLE.mut.germline.as.missing %in% ALL.MISSING.CODES &
           !data$init.treatment %in% ALL.MISSING.CODES, ],
    var.name = "POLE.x.init.treatment", var.description = "POLE mutation status x any adjuvant treatment",
    line.color = c("black", "black", "red", "red"), line.width = c(3, 1, 3, 1),
    single.test.type = "none", surv.type = surv.type, 
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
}

#' Grade trend barplot
#' 
#' @param input.d input \code{data.frame}
#' @param title title of barplot
#' @param legend.space amount of space between legend and edge of plot
#' @param use.prop logical; if \code{TRUE}, proportions are used
#' @return a barplot showing the change of Grade over time
#' @author Samuel Leung
#' @export
doGrdTrendBarplot <- function(input.d, title, legend.space = 0.2,
                              use.prop = TRUE) {
  legend.space <- round(legend.space * nrow(input.d))
  grade.trend <- grade.trend.n <- c()
  temp.d <- input.d
  dx.years <- names(table(temp.d$dx.year))
  for (dx.year in dx.years) {
    select.cases <- temp.d$dx.year == dx.year
    n <- length(temp.d$Tumour.Grade[select.cases])
    grade1 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 1")	
    grade2 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 2")
    grade3 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 3")
    grade <- c(grade1, grade2, grade3)
    if (use.prop)
      grade <- grade / n * 100
    grade.trend <- cbind(grade.trend, grade)
    grade.trend.n <- c(grade.trend.n, n)
  }
  colnames(grade.trend) <- dx.years
  if (length(dx.years) > 20)
    colnames(grade.trend)[which(rep(c(FALSE, TRUE), 11))] <- ""
  if (sum(dx.years == 1999) > 0)
    colnames(grade.trend)[dx.years == 1999] <- ""
  if (sum(dx.years == 2000) > 0)
    colnames(grade.trend)[dx.years == 2000] <- 2000
  if (sum(dx.years == 2001) > 0)
    colnames(grade.trend)[dx.years == 2001] <- ""
  rownames(grade.trend) <- c("Grade 1", "Grade 2", "Grade 3")
  grade.trend <- cbind(grade.trend, rep("", nrow(grade.trend)))
  colnames(grade.trend)[ncol(grade.trend)] <- ""
  grade.trend.n <- c(grade.trend.n, legend.space)
  barplot(grade.trend, width = grade.trend.n, legend.text = T,
          args.legend = list(x = "right"),
          col = c("red4", "red3", "red"), las = 2, main = title,
          ylab = "Percentage of cases", xlab = "Year of diagnosis",
          sub = "(width of bar proportional to the number of patients)")
}

#' Survival trend barplot
#' 
#' @param input.d input \code{data.frame}
#' @param title title of barplot
#' @param surv.yrs vector of survival times in years
#' @param surv.sts vector of survival statuses
#' @param surv.event vector of survival events
#' @param event.desc description of events
#' @param censor.desc description of censoring
#' @param obs.yrs number of years to observe survival trend
#' @param legend.space amount of space between legend and edge of plot
#' @param use.prop logical; if \code{TRUE}, proportions are used
#' @return a barplot showing the change of survival outcome over time
#' @author Samuel Leung
#' @export
doSurvTrendBarplot <- function(input.d, title, surv.yrs, surv.sts, surv.event,
                               event.desc, censor.desc,  obs.yrs = 3,
                               legend.space = 0.2, use.prop = TRUE) {
  legend.space <- round(legend.space * nrow(input.d))
  survival.trend <- survival.trend.n <- c()
  temp.d <- input.d
  dx.years <- names(table(temp.d$dx.year))
  for (dx.year in dx.years) {
    select.cases <- temp.d$dx.year == dx.year
    n <- sum(!is.na(temp.d[select.cases, surv.yrs]))
    event <- sum(temp.d[select.cases, surv.sts] == surv.event &
                   as.numeric(temp.d[select.cases, surv.yrs]) <= obs.yrs,
                 na.rm = TRUE)
    censor <- n - event
    survival <- c(event, censor)
    if (use.prop)
      survival <- survival / n * 100
    survival.trend <- cbind(survival.trend, survival)
    survival.trend.n <- c(survival.trend.n, n)
  }
  colnames(survival.trend) <- dx.years
  if (length(dx.years) > 20)
    colnames(survival.trend)[which(rep(c(FALSE, TRUE), 11))] <- ""
  if (sum(dx.years == 1999) > 0)
    colnames(survival.trend)[dx.years == 1999] <- ""
  if (sum(dx.years == 2000) > 0)
    colnames(survival.trend)[dx.years == 2000] <- 2000
  if (sum(dx.years == 2001) > 0)
    colnames(survival.trend)[dx.years == 2001] <- ""
  rownames(survival.trend) <- c(event.desc, censor.desc)
  survival.trend <- cbind(survival.trend, rep("", nrow(survival.trend)))
  colnames(survival.trend)[ncol(survival.trend)] <- ""
  survival.trend.n <- c(survival.trend.n, legend.space)
  barplot(survival.trend, width = survival.trend.n, legend.text = TRUE,
          args.legend = list(x = "right"), col = c("grey", "pink"),
          las = 2, main = title,
          ylab = "Percentage of cases", xlab = "Year of diagnosis",
          sub = "(width of bar proportional to the number of patients)")
}

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