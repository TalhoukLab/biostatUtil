#' Do check PH function
#' @export
doCheckPH <- function(var.name,var.description) {
  
  temp.d <- emdb[!sapply(emdb[,var.name],as.character) %in% ALL.MISSING.CODES,]
  temp.d[,var.name] <- as.numeric(temp.d[,var.name])
  
  # OS
  temp.d$os.yrs  <- as.numeric(temp.d$os.yrs)
  test <- pretty.coxph(as.formula(paste("Surv(os.yrs, os.sts=='os.event'  ) ~",var.name)), input.d=temp.d, check.ph=TRUE, ph.test.plot.filename=NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main="OS")
  
  # DSS
  temp.d$dss.yrs  <- as.numeric(temp.d$dss.yrs)
  test <- pretty.coxph(as.formula(paste("Surv(dss.yrs, dss.sts=='dss.event'  ) ~",var.name)), input.d=temp.d[!is.na(temp.d$dss.sts),], check.ph=TRUE, ph.test.plot.filename=NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main="DSS")
  
  # RFS
  temp.d$rfs.yrs  <- as.numeric(temp.d$rfs.yrs)
  test <- pretty.coxph(as.formula(paste("Surv(rfs.yrs, rfs.sts=='rfs.event'  ) ~",var.name)), input.d=temp.d[!is.na(temp.d$rfs.sts) & !is.na(temp.d$rfs.yrs),], check.ph=TRUE, ph.test.plot.filename=NA)
  dimnames(test$ph.test$y)[[2]] <- rep(var.description,length(dimnames(test$ph.test$y)[[2]]))
  plot(test$ph.test, main="RFS")
}