#' Check Cox proportional hazards assumption
#' 
#' Use Schoenfeld residuals to test the Cox proportional hazards assumption in a
#' (multivariable) survival model.
#' 
#' \code{doCheckPH} is intended for use in univariable models, and
#' \code{doCheckPHMulti} is intended for use in multivariable models.
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
  
  temp.d$os.yrs  <- as.numeric(temp.d$os.yrs)
  test <- prettyCoxph(as.formula(paste("Surv(os.yrs, os.sts=='os.event'  ) ~",
                                       var.name)),
                      temp.d,
                      check.ph = TRUE, ph.test.plot.filename = NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,
                                       length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main = "OS")
  
  temp.d$dss.yrs <- as.numeric(temp.d$dss.yrs)
  test <- prettyCoxph(as.formula(paste("Surv(dss.yrs, dss.sts=='dss.event'  ) ~",
                                       var.name)),
                      temp.d[!is.na(temp.d$dss.sts), ],
                      check.ph = TRUE, ph.test.plot.filename = NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,
                                       length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main = "DSS")
  
  temp.d$rfs.yrs <- as.numeric(temp.d$rfs.yrs)
  test <- prettyCoxph(as.formula(paste("Surv(rfs.yrs, rfs.sts=='rfs.event'  ) ~",
                                       var.name)),
                      temp.d[!is.na(temp.d$rfs.sts) & !is.na(temp.d$rfs.yrs), ],
                      check.ph = TRUE, ph.test.plot.filename = NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,
                                       length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main = "PFS")
}

#' @inheritParams doCheckPH
#' @rdname doCheckPH
#' @export
doCheckPHMulti <- function(data, var.name, var.description) {
  test <- prettyCoxph(as.formula(paste("Surv(os.yrs, os.sts=='os.event'  ) ~",
                                       paste(var.name, collapse = " + "))),
                      input.d = data,
                      check.ph = TRUE, ph.test.plot.filename = NA)
  plot(test$ph.test, var = 1, ann = F)
  title(main = "OS", xlab = "Year", ylab = paste("Beta(t) for", var.description))
  
  test <- prettyCoxph(as.formula(paste("Surv(dss.yrs, dss.sts=='dss.event'  ) ~",
                                       paste(var.name, collapse = " + "))),
                      input.d = data[!is.na(data$dss.sts), ],
                      check.ph = TRUE, ph.test.plot.filename = NA)
  plot(test$ph.test, var = 1, ann = F)
  title(main = "DSS", xlab = "Year", ylab = paste("Beta(t) for", var.description))
  
  test <- prettyCoxph(as.formula(paste("Surv(rfs.yrs, rfs.sts=='rfs.event'  ) ~",
                                       paste(var.name, collapse = " + "))),
                      input.d = data[!is.na(data$rfs.sts) &
                                       !is.na(data$rfs.yrs), ],
                      check.ph = TRUE, ph.test.plot.filename = NA)
  plot(test$ph.test, var = 1, ann = F)
  title(main = "PFS", xlab = "Year", ylab = paste("Beta(t) for", var.description))
}