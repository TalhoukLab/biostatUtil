#' Make Kaplan-Meier plots for POLE
doKMPlotsPOLE <- function(surv.type, use.aline.plot = FALSE) {
  doKMPlots(
    emdb[!emdb$POLE.mut.germline.as.missing%in%ALL.MISSING.CODES,],
    "POLE.mut.germline.as.missing", 
    "POLE mutation status - whole cohort",
    single.test.type="logrank",
    surv.type=surv.type,
    conf.int=TRUE, # per missing 2015-01-09 ... wanted confidence interval on KM plots
    use.aline.plot=use.aline.plot
  )
  doKMPlots(
    emdb[!emdb$POLE.mut.germline.as.missing%in%ALL.MISSING.CODES & !emdb$init.treatment%in%ALL.MISSING.CODES & emdb$init.treatment=="no.treatment",],
    "POLE.mut.germline.as.missing", "POLE mutation status - no adjuvant treatment",
    single.test.type="logrank",
    surv.type=surv.type,
    conf.int=TRUE,
    use.aline.plot=use.aline.plot
  )
  doKMPlots(
    emdb[!emdb$POLE.mut.germline.as.missing%in%ALL.MISSING.CODES & !emdb$init.treatment%in%ALL.MISSING.CODES & emdb$init.treatment!="no.treatment",],
    "POLE.mut.germline.as.missing", "POLE mutation status - any adjuvant treatment",
    single.test.type="logrank",
    surv.type=surv.type,
    conf.int=TRUE,
    use.aline.plot=use.aline.plot
  )
}