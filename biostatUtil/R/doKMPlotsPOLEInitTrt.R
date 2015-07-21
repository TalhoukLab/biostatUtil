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
    "POLE.x.init.treatment", "POLE mutation status x any adjuvant treatment",
    line.color = c("black", "black", "red", "red"), line.width = c(3, 1, 3, 1),
    single.test.type = "none", surv.type = surv.type, 
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
}