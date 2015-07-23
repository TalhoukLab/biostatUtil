#' Make Kaplan-Meier plots
#' 
#' @param input.d \code{data.frame} containing data
#' @param var.name name of variable to make Kaplan-Meier plots on
#' @param var.description description for \code{var.name}
#' @param line.color colors for survival curves
#' @param line.pattern line type for survival curves
#' @param km.plot.ref.group specify KM plot reference group; "single" means a lump
#' log-rank statistic
#' @param single.test.type test to use for survival curves. Defaults to "logrank".
#' @param surv.type survival outcome. Either "os", "dss", or "rfs".
#' @param use.firth Whether to use Firth's correction for plotting the curves
#' @param use.aline.plot if \code{TRUE}, will use Aline's plot function
#' @param ... additional arguments to other functions and methods
#' @return A Kaplan-Meier plot for the specified survival outcome split on the desired
#' variable.
#' @author Samuel Leung
#' @export
doKMPlots <- function(input.d, var.name, var.description, line.color = NULL,
                      line.pattern = NULL, km.plot.ref.group = "single",
                      single.test.type = "logrank", surv.type = "os",
                      use.firth = FIRTH.THRESHOLD, use.aline.plot = FALSE,
                      ...) {
  pos <- 1
  if (is.factor(input.d[, var.name])) {
    input.d[, var.name] <- droplevels(input.d[, var.name])
  }
  temp.d <- input.d
  temp.d$os.yrs <- as.numeric(temp.d$os.yrs)
  if (is.null(line.color)) {
    line.color <- c(1:length(names(table(temp.d[, var.name]))))
  }
  if (is.null(line.pattern)) {
    line.pattern <- 1
  }
  if (surv.type == "os") {
    formula.obj <- as.formula(paste("Surv(os.yrs,os.sts=='os.event') ~", var.name))
    sfit <- survival::survfit(formula.obj, data = temp.d)
    # set local variable in environment searchable by local function calls
    assign("formula.obj", formula.obj, envir = as.environment(pos)) 
    assign("temp.d", temp.d, envir = as.environment(pos)) 
    
    if (!use.aline.plot) {
      plotKM(temp.d,
              formula.obj, #as.formula(paste("Surv(os.yrs,os.sts=='os.event') ~",var.name)),
              paste0(var.description, " (OS)"),
              OS.XLAB, # x-axis label
              OS.YLAB, # y-axis label
              names(table(temp.d[, var.name])), # line names
              line.color,
              line.pattern = line.pattern,
              show.test = km.plot.ref.group,   # show single or the reference group value (for pairwise comparison)
              # none = no test shown
              single.test.type = single.test.type, # the test to show if specified show.test="single". 
              # the possible choices are logrank, wilcoxon, taroneware, all
              obs.survyrs = 3,
              ...
      )
    } else {	
      ggkm(sfit, sfit2 = NULL,
           table = TRUE,
           returns = FALSE,
           marks = TRUE,
           xlabs = OS.XLAB, #"Time",
           ylabs = OS.YLAB, #"Survival Probability",
           xlims = c(0, max(sfit$time)),
           ylims = c(0, 1),
           ystratalabs = names(table(temp.d[, var.name])),#NULL,
           ystrataname = NULL,
           timeby = 1,
           main = paste0(var.description, " (OS)"),#"Kaplan-Meier Plot",
           pval = TRUE,
           HR = TRUE,
           use.firth = use.firth,
           CI = TRUE,
           subs = NULL,
           legend = FALSE,
           ...)	
    }
  } else if (surv.type == "dss") {
    temp.d <- input.d[!is.na(input.d$dss.sts), ]
    temp.d$dss.yrs <- as.numeric(temp.d$dss.yrs)
    
    formula.obj <- as.formula(paste("Surv(dss.yrs,dss.sts=='dss.event') ~",
                                    var.name))
    sfit <- survival::survfit(formula.obj, data = temp.d)
    # set local variable in environment searchable by local function calls
    assign("formula.obj", formula.obj, envir = as.environment(pos)) 
    assign("temp.d", temp.d, envir = as.environment(pos)) 
    
    if (!use.aline.plot) {
      dss.stats <- plotKM(temp.d,
                           formula.obj, #as.formula(paste("Surv(dss.yrs,dss.sts=='dss.event') ~",var.name)),
                           paste0(var.description, " (DSS)"),
                           DSS.XLAB,
                           DSS.YLAB,
                           names(table(temp.d[, var.name])),
                           line.color,
                           line.pattern = line.pattern,
                           show.test = km.plot.ref.group,   # show single or the reference group value (for pairwise comparison)
                           # none = no test shown
                           single.test.type = single.test.type, # the test to show if specified show.test="single". 
                           # the possible choices are logrank, wilcoxon, taroneware, all
                           obs.survyrs = 3,
                           ... 
      )
    } else {
      ggkm(sfit, sfit2 = NULL,
           table = TRUE,
           returns = FALSE,
           marks = TRUE,
           xlabs = DSS.XLAB, #"Time",
           ylabs = DSS.YLAB, #"Survival Probability",
           xlims = c(0, max(sfit$time)),
           ylims = c(0, 1),
           ystratalabs = names(table(temp.d[, var.name])),#NULL,
           ystrataname = NULL,
           timeby = 1,
           main = paste0(var.description, " (DSS)"),#"Kaplan-Meier Plot",
           pval = TRUE,
           HR = TRUE,
           use.firth = use.firth,
           CI = TRUE,
           subs = NULL,
           legend = FALSE,
           ...)	
    }
  } else if (surv.type == "rfs") {
    temp.d <- input.d[!is.na(input.d$rfs.sts) & !is.na(input.d$rfs.yrs), ]
    temp.d$rfs.yrs <- as.numeric(temp.d$rfs.yrs)
    
    formula.obj <- as.formula(paste("Surv(rfs.yrs,rfs.sts=='rfs.event') ~",
                                    var.name))
    sfit <- survival::survfit(formula.obj, data = temp.d)
    # set local variable in environment searchable by local function calls
    assign("formula.obj", formula.obj, envir = as.environment(pos)) 
    assign("temp.d", temp.d, envir = as.environment(pos)) 
     
    if (!use.aline.plot) {
      rfs.stats <- plotKM(temp.d,
                           formula.obj, #as.formula(paste("Surv(rfs.yrs,rfs.sts=='rfs.event') ~",var.name)),
                           paste0(var.description," (RFS)"),
                           RFS.XLAB,
                           RFS.YLAB,
                           names(table(temp.d[, var.name])),
                           line.color,
                           line.pattern = line.pattern,
                           show.test = km.plot.ref.group,   # show single or the reference group value (for pairwise comparison)
                           # none = no test shown
                           single.test.type = single.test.type, # the test to show if specified show.test="single". 
                           # the possible choices are logrank, wilcoxon, taroneware, all
                           obs.survyrs = 3,
                           ... 
      )
    } else {
      ggkm(sfit, sfit2 = NULL,
           table = TRUE,
           returns = FALSE,
           marks = TRUE,
           xlabs = RFS.XLAB, #"Time",
           ylabs = RFS.YLAB, #"Survival Probability",
           xlims = c(0, max(sfit$time)),
           ylims = c(0, 1),
           ystratalabs = names(table(temp.d[, var.name])),#NULL,
           ystrataname = NULL,
           timeby = 1,
           main = paste0(var.description," (PFS)"),#"Kaplan-Meier Plot",
           pval = TRUE,
           HR = TRUE,
           use.firth = use.firth,
           CI = TRUE,
           subs = NULL,
           legend = FALSE,
           ...)	
    }
  }
}