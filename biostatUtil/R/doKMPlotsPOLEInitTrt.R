#' Make Kaplan-Meier plots with initial treatment
#' @export
doKMPlotsPOLEInitTrt <- function(surv.type, use.aline.plot = FALSE # use Aline's plot function
) {
  doKMPlots(
    emdb[!emdb$POLE.mut.germline.as.missing%in%ALL.MISSING.CODES & !emdb$init.treatment%in%ALL.MISSING.CODES,],
    "POLE.x.init.treatment", "POLE mutation status x any adjuvant treatment",
    line.color=c("black","black","red","red"),
    line.width=c(3,1,3,1),
    single.test.type="none",
    surv.type=surv.type, 
    conf.int=TRUE,
    use.aline.plot=use.aline.plot
  )
}