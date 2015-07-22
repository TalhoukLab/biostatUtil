# POLE paper - Remark #18 (further investigation including checking model assumption)
# e.g. check assumption 
#
# Author: samuelc
###############################################################################

# function to do schonfeld residual plots
do.check.ph <- function(input.d,var.name,var.description, ref.group=NA) {
	
	temp.d <- input.d[!sapply(input.d[,var.name],as.character) %in% ALL.MISSING.CODES,]
	
	levels <- NA
	if (is.na(ref.group)) {
		# assume variable is numeric or binary
		temp.d[,var.name] <- as.numeric(temp.d[,var.name])
	} else {
		levels <- names(table(temp.d[,var.name]))
		levels <- c(ref.group,levels[levels != ref.group])
		temp.d[,var.name] <- factor(sapply(temp.d[,var.name],as.character),levels=levels)
	}
	
	# OS
	temp.d$os.yrs  <- as.numeric(temp.d$os.yrs)
	test <- biostatUtil::prettyCoxph(as.formula(paste("Surv(os.yrs, os.sts=='os.event'  ) ~",var.name)), input.d=temp.d, check.ph=TRUE, ph.test.plot.filename=NA)
	dimnames(test$ph.test$y)[[2]] <- rep(var.description,length(dimnames(test$ph.test$y)[[2]]))
	if (length(levels)==1) {
		plot(test$ph.test, main="OS")
	} else {
		for (i in 2:(length(levels))) {
			plot(test$ph.test[i-1],main=paste(ifelse(i==2,"OS\n","\n"),levels[i]," vs. ",ref.group," (ref)",sep=""))
		}	
	}
	
	# DSS
	temp.d$dss.yrs  <- as.numeric(temp.d$dss.yrs)
	test <- biostatUtil::prettyCoxph(as.formula(paste("Surv(dss.yrs, dss.sts=='dss.event'  ) ~",var.name)), input.d=temp.d[!is.na(temp.d$dss.sts),], check.ph=TRUE, ph.test.plot.filename=NA)
	dimnames(test$ph.test$y)[[2]] <- rep(var.description,length(dimnames(test$ph.test$y)[[2]]))
	if (length(levels)==1) {
		plot(test$ph.test, main="DSS")
	} else {
		for (i in 2:(length(levels))) {
			plot(test$ph.test[i-1],main=paste(ifelse(i==2,"DSS\n","\n"),levels[i]," vs. ",ref.group," (ref)",sep=""))
		}	
	}
	
	# RFS
	temp.d$rfs.yrs  <- as.numeric(temp.d$rfs.yrs)
	test <- biostatUtil::prettyCoxph(as.formula(paste("Surv(rfs.yrs, rfs.sts=='rfs.event'  ) ~",var.name)), input.d=temp.d[!is.na(temp.d$rfs.sts) & !is.na(temp.d$rfs.yrs),], check.ph=TRUE, ph.test.plot.filename=NA)
	dimnames(test$ph.test$y)[[2]] <- rep(var.description,length(dimnames(test$ph.test$y)[[2]]))
	if (length(levels)==1) {
		plot(test$ph.test, main="RFS")
	} else {
		for (i in 2:(length(levels))) {
			plot(test$ph.test[i-1],main=paste(ifelse(i==2,"RFS\n","\n"),levels[i]," vs. ",ref.group," (ref)",sep=""))
		}	
	}
	
}
