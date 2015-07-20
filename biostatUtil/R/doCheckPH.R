#' Check Cox proportional hazards assumption
#' 
#' Use Schoenfeld residuals to test the Cox proportional hazards assumption.
#' 
#' @param data the data object
#' @param var.name column name of variable to test in \code{data}
#' @param var.description label to use for \code{var.name} in plots
#' @return Schoenfeld residual plots for each survival outcome
#' @author Samuel Leung
#' @export
doCheckPH <- function(data, var.name, var.description) {
  
  temp.d <- data[!sapply(data[, var.name], as.character) %in%
                   ALL.MISSING.CODES, ]
  temp.d[, var.name] <- as.numeric(temp.d[, var.name])
  
  # OS
  temp.d$os.yrs  <- as.numeric(temp.d$os.yrs)
  test <- prettyCoxph(as.formula(paste("Surv(os.yrs, os.sts=='os.event'  ) ~",
                                        var.name)),
                       input.d = temp.d,
                       check.ph = TRUE, ph.test.plot.filename = NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,
                                       length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main = "OS")
  
  # DSS
  temp.d$dss.yrs <- as.numeric(temp.d$dss.yrs)
  test <- prettyCoxph(as.formula(paste("Surv(dss.yrs, dss.sts=='dss.event'  ) ~",
                                        var.name)),
                       input.d = temp.d[!is.na(temp.d$dss.sts), ],
                       check.ph = TRUE, ph.test.plot.filename = NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,
                                       length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main = "DSS")
  
  # RFS
  temp.d$rfs.yrs <- as.numeric(temp.d$rfs.yrs)
  test <- prettyCoxph(as.formula(paste("Surv(rfs.yrs, rfs.sts=='rfs.event'  ) ~",
                                        var.name)),
                       input.d = temp.d[!is.na(temp.d$rfs.sts) & !is.na(temp.d$rfs.yrs), ],
                       check.ph = TRUE, ph.test.plot.filename = NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,
                                       length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main = "RFS")
}