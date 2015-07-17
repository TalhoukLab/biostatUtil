#' Make Kaplan-Meier plots for POLE
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
           data$init.treatment == "no.treatment", ],
    "POLE.mut.germline.as.missing",
    "POLE mutation status - no adjuvant treatment",
    single.test.type = "logrank", surv.type = surv.type,
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
  
  doKMPlots(
    data[!data$POLE.mut.germline.as.missing %in% ALL.MISSING.CODES &
           !data$init.treatment %in% ALL.MISSING.CODES &
           data$init.treatment != "no.treatment", ],
    "POLE.mut.germline.as.missing",
    "POLE mutation status - any adjuvant treatment",
    single.test.type = "logrank", surv.type = surv.type,
    conf.int = TRUE, use.aline.plot = use.aline.plot, ...)
}