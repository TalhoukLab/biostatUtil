#' Check the proportional hazards assumption in multivariable model
#' 
#' Use Schoenfeld residuals to test the Cox proportional hazards assumption.
#' 
#' @param data the data object
#' @param var.name column name of variable to test in \code{data}
#' @param var.description label to use for \code{var.name} in plots
#' @return Schoenfeld residual plots for each survival outcome
#' @author Samuel Leung
#' @export
doCheckPHMulti <- function(data, var.name, var.description) {
  # OS
  test <- prettyCoxph(as.formula(paste("Surv(os.yrs, os.sts=='os.event'  ) ~",
                                       paste(var.name, collapse = " + "))),
                      input.d = data,
                      check.ph = TRUE, ph.test.plot.filename = NA)
  plot(test$ph.test, var = 1, ann = F)
  title(main = "OS", xlab = "Year", ylab = paste("Beta(t) for", var.description))
  
  # DSS
  test <- prettyCoxph(as.formula(paste("Surv(dss.yrs, dss.sts=='dss.event'  ) ~",
                                       paste(var.name, collapse = " + "))),
                      input.d = data[!is.na(data$dss.sts), ],
                      check.ph = TRUE, ph.test.plot.filename = NA)
  plot(test$ph.test, var = 1, ann = F)
  title(main = "DSS", xlab = "Year", ylab = paste("Beta(t) for", var.description))
  
  # RFS
  test <- prettyCoxph(as.formula(paste("Surv(rfs.yrs, rfs.sts=='rfs.event'  ) ~",
                                       paste(var.name, collapse = " + "))),
                      input.d = data[!is.na(data$rfs.sts) &
                                       !is.na(data$rfs.yrs), ],
                      check.ph = TRUE, ph.test.plot.filename = NA)
  plot(test$ph.test, var = 1, ann = F)
  title(main = "PFS", xlab = "Year", ylab = paste("Beta(t) for", var.description))
}