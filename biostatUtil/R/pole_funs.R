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