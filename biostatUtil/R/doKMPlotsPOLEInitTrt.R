#' Make Kaplan-Meier plots with initial treatment
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