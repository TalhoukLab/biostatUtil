#' Make Kaplan-Meier plots
#' 
#' @param input.d \code{data.frame} containing data
#' @param time follow up time
#' @param status status indicator
#' @param var.name name of variable to make Kaplan-Meier plots on
#' @param var.description description for \code{var.name}
#' @param line.name names for each survival curve
#' @param line.color colors for survival curves
#' @param line.pattern line type for survival curves
#' @param km.plot.ref.group specify KM plot reference group; "single" means a lump
#' log-rank statistic
#' @param single.test.type test to use for survival curves. Defaults to "logrank".
#' @param surv.type survival outcome. Either "os", "dss", or "pfs".
#' @param use.firth Whether to use Firth's correction for plotting the curves
#' @param use.aline.plot if \code{TRUE}, will use Aline's plot function
#' @param ... additional arguments to other functions and methods
#' @return A Kaplan-Meier plot for the specified survival outcome split on the desired
#' variable.
#' @author Samuel Leung, Derek Chiu
#' @export
doKMPlots <- function(input.d, time, status, var.name, var.description,
                      line.name = NULL, line.color = NULL, line.pattern = NULL,
                      km.plot.ref.group = "single",
                      single.test.type = "logrank", surv.type = "os",
                      use.firth = FIRTH.THRESHOLD,
                      use.aline.plot = FALSE, ...) {
  pos <- 1
  if (is.null(line.name))
    line.name <- names(table(input.d[, var.name])) 
  if (is.null(line.color))
    line.color <- c(1:length(names(table(input.d[, var.name]))))
  if (is.null(line.pattern))
    line.pattern <- 1
  if (is.factor(input.d[, var.name]))
    input.d[, var.name] <- droplevels(input.d[, var.name])
  temp.d <- input.d
  temp.d[, time] <- as.numeric(temp.d[, time])
  formula.obj <- as.formula(paste0("Surv(", time, ", ", status, ") ~ ",
                                   var.name))
  sfit <- survival::survfit(formula.obj, data = temp.d)
  assign("formula.obj", formula.obj, envir = as.environment(pos)) 
  assign("temp.d", temp.d, envir = as.environment(pos)) 
  if (!use.aline.plot) {
    plotKM(temp.d, formula.obj,
           line.name, line.color, line.pattern = line.pattern, 
           main=paste0(var.description, " (", toupper(surv.type), ")"),
           show.test = km.plot.ref.group, single.test.type = single.test.type,
           obs.survyrs = 3, ...)
  } else {	
    ggkm(sfit, sfit2 = NULL, table = TRUE, returns = FALSE, marks = TRUE,
         xlims = c(0, max(sfit$time)), ylims = c(0, 1),
         ystratalabs = line.name, ystrataname = NULL, timeby = 1,
         main = paste0(var.description, " (", toupper(surv.type), ")"),
         pval = TRUE, HR = TRUE, use.firth = use.firth, CI = TRUE, subs = NULL,
         legend = FALSE, ...)	
  }
}
