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
    "POLE.mut.germline.as.missing", "POLE mutation status - whole cohort",
    single.test.type = "logrank", surv.type = surv.type,
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
  
  doKMPlots(
    data[!data$POLE.mut.germline.as.missing %in% ALL.MISSING.CODES &
           !data$init.treatment %in% ALL.MISSING.CODES &
           data$init.treatment == VALUE.CODING.INIT.TREATMENT.NO, ],
    "POLE.mut.germline.as.missing",
    "POLE mutation status - no adjuvant treatment",
    single.test.type = "logrank", surv.type = surv.type,
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
  
  doKMPlots(
    data[!data$POLE.mut.germline.as.missing %in% ALL.MISSING.CODES &
           !data$init.treatment %in% ALL.MISSING.CODES &
           data$init.treatment != VALUE.CODING.INIT.TREATMENT.NO, ],
    "POLE.mut.germline.as.missing",
    "POLE mutation status - any adjuvant treatment",
    single.test.type = "logrank", surv.type = surv.type,
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
}