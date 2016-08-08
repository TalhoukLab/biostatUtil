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
             cox1 = coxph(Surv(DSS.yrs, DSS.sts =="dss.event") ~
                            POLE, data = dmat)         
             cox2 = coxph(Surv(DSS.yrs, DSS.sts =="dss.event") ~
                            POLE + Rx, data = dmat)
             cox3 = coxph(Surv(DSS.yrs, DSS.sts =="dss.event") ~
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