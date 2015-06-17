# POLE paper - Remark #17: full multivariable model regardless of their significance
# 
# Author: samuelc
###############################################################################
if (!exists("RUN.IN.MARKDOWN")) {
	# if variable does not exist, that means we are not running in the markdown file
	RUN.IN.MARKDOWN <- FALSE # indicate whether we are running the files in the marked down file
	# do set up if not fun in markdown
	# this is for code development & testing ... so assume this will never get called from 
	# Aline's computer
	source('remark_setup.R')
}

multi.cox.summary <- do.coxph.multivariable( # a table of univariable cox models of POLE and other variables
	emdb,
	c(
		"POLE.mut.germline.as.missing",
		"age.at.surgery",
		"stage_b1v234",
		"Tumour.Grade_b12v3",
		"Histological.Subtype_non_endo",
		"LVSI",
		"any.positive.nodes",
		"any.init.treatment"
	), 
	c(
		HTML.COX.VAR.DESC.POLE,
		HTML.COX.VAR.DESC.AGE,
		HTML.COX.VAR.DESC.STAGE,
		HTML.COX.VAR.DESC.GRADE,
		HTML.COX.VAR.DESC.HIST,
		HTML.COX.VAR.DESC.LVSI,
		HTML.COX.VAR.DESC.ANY.POS.NODES,
		HTML.COX.VAR.DESC.ANY.INIT.ADJ.TX
	),
	use.firth=FIRTH.THRESHOLD,
	stat.test="logtest",
	var.ref.groups=c(POLE.REF.GROUP,NA,NA,NA,NA,NA,NA,NA),
	caption=paste("Multivariable analyses of POLE (exclude BMI)",sep=""),
	banded.rows=TRUE
)

# manual test ...
if (FALSE) {
	multi.formula <- Surv(dss.yrs, dss.sts=="dss.event") ~ POLE.mut.consolidated.numeric + age.at.surgery + stage_b1v234 + Histological.Subtype_non_endo + LVSI + any.positive.nodes + any.init.treatment
	multi.formula.minus.pole <- Surv(dss.yrs, dss.sts=="dss.event") ~ age.at.surgery + stage_b1v234 + Histological.Subtype_non_endo + LVSI + any.positive.nodes + any.init.treatment

	temp.d <- emdb[
		emdb$Tumour.Grade == "Grade 3" &	
		!is.na(emdb$age.at.surgery) &
		!emdb$stage_b1v234 %in% ALL.MISSING.CODES &
		!emdb$LVSI %in% ALL.MISSING.CODES &
		!emdb$any.positive.nodes %in% ALL.MISSING.CODES &
		!is.na(emdb$any.init.treatment),
	]
	temp.d$stage_b1v234 <- sapply(temp.d$stage_b1v234,as.character)
	temp.d$stage_b1v234 <- factor(temp.d$stage_b1v234,names(table(temp.d$stage_b1v234)))
	temp.d$LVSI <- sapply(temp.d$LVSI,as.character)
	temp.d$LVSI <- factor(temp.d$LVSI,names(table(temp.d$LVSI)))
	temp.d$any.positive.nodes <- sapply(temp.d$any.positive.nodes,as.character)
	temp.d$any.positive.nodes <- factor(temp.d$any.positive.nodes,names(table(temp.d$any.positive.nodes)))

	temp.d$stage_b1v234 <- as.numeric(temp.d$stage_b1v234)
	temp.d$Histological.Subtype_non_endo <- as.numeric(temp.d$Histological.Subtype_non_endo)
	temp.d$LVSI <- as.numeric(temp.d$LVSI)
	temp.d$any.positive.nodes <- as.numeric(temp.d$any.positive.nodes)
	temp.d$any.init.treatment <- as.numeric(temp.d$any.init.treatment)

	temp.d <- temp.d[!is.na(temp.d$dss.sts),]
	coxphf(multi.formula,data=temp.d)

	anova(coxph(multi.formula,data=temp.d), coxph(multi.formula.minus.pole,data=temp.d))
	coxphftest(multi.formula, data=temp.d, test=~age.at.surgery + stage_b1v234 + Histological.Subtype_non_endo + LVSI + any.positive.nodes + any.init.treatment)
	coxphf(multi.formula,data=temp.d)
}


multi.cox.summary.g3 <- do.coxph.multivariable( 
		emdb[emdb$Tumour.Grade=="Grade 3",], 
		c(
				"POLE.mut.germline.as.missing",
				"age.at.surgery",
				"stage_b1v234",
				"Histological.Subtype_non_endo",
				"LVSI",
				"any.positive.nodes",
				"any.init.treatment"
		), 
		c(
				HTML.COX.VAR.DESC.POLE,
				HTML.COX.VAR.DESC.AGE,
				HTML.COX.VAR.DESC.STAGE,
				HTML.COX.VAR.DESC.HIST,
				HTML.COX.VAR.DESC.LVSI,
				HTML.COX.VAR.DESC.ANY.POS.NODES,
				HTML.COX.VAR.DESC.ANY.INIT.ADJ.TX
		),
		use.firth=FIRTH.THRESHOLD, 
		stat.test="logtest",
		var.ref.groups=c(POLE.REF.GROUP,NA,NA,NA,NA,NA,NA),
		caption=paste("Multivariable analyses of POLE (exclude BMI) among grade 3",sep=""),
		banded.rows=TRUE
)


#######################################
# per suggesting from Steve McKinney, try bootstrap Firth hazard ratio and see if ci 
# of confidence interval crosses one
WRITE.RESULTS.TO.FILE <- FALSE # it takes very long to do all the Firth models
num.boot <- 200 # number of bootstrap samples to try

if (WRITE.RESULTS.TO.FILE) {
	multi.formula.os  <- Surv(os.yrs,  os.sts =="os.event" ) ~ POLE.mut.consolidated.numeric + age.at.surgery + stage_b1v234 + Histological.Subtype_non_endo + LVSI + any.positive.nodes + any.init.treatment
	multi.formula.dss <- Surv(dss.yrs, dss.sts=="dss.event") ~ POLE.mut.consolidated.numeric + age.at.surgery + stage_b1v234 + Histological.Subtype_non_endo + LVSI + any.positive.nodes + any.init.treatment
	multi.formula.rfs <- Surv(rfs.yrs, rfs.sts=="rfs.event") ~ POLE.mut.consolidated.numeric + age.at.surgery + stage_b1v234 + Histological.Subtype_non_endo + LVSI + any.positive.nodes + any.init.treatment


	cox.d <- emdb[
		emdb$Tumour.Grade == "Grade 3" &	
		!is.na(emdb$age.at.surgery) &
		!emdb$stage_b1v234 %in% ALL.MISSING.CODES &
		!emdb$LVSI %in% ALL.MISSING.CODES &
		!emdb$any.positive.nodes %in% ALL.MISSING.CODES &
		!is.na(emdb$any.init.treatment),
	]

	cox.d$stage_b1v234 <- sapply(cox.d$stage_b1v234,as.character)
	cox.d$stage_b1v234 <- factor(cox.d$stage_b1v234,names(table(cox.d$stage_b1v234)))
	cox.d$LVSI <- sapply(cox.d$LVSI,as.character)
	cox.d$LVSI <- factor(cox.d$LVSI,names(table(cox.d$LVSI)))
	cox.d$any.positive.nodes <- sapply(cox.d$any.positive.nodes,as.character)
	cox.d$any.positive.nodes <- factor(cox.d$any.positive.nodes,names(table(cox.d$any.positive.nodes)))

	cox.d$stage_b1v234 <- as.numeric(cox.d$stage_b1v234)
	cox.d$Histological.Subtype_non_endo <- as.numeric(cox.d$Histological.Subtype_non_endo)
	cox.d$LVSI <- as.numeric(cox.d$LVSI)
	cox.d$any.positive.nodes <- as.numeric(cox.d$any.positive.nodes)
	cox.d$any.init.treatment <- as.numeric(cox.d$any.init.treatment)

	cox.dss.d <- cox.d[!is.na(cox.d$dss.sts),]
	cox.rfs.d <- cox.d[!is.na(cox.d$rfs.yrs),]

	boot.hrs.os <- rep(NA,num.boot)
	boot.hrs.dss <- rep(NA,num.boot)
	boot.hrs.rfs <- rep(NA,num.boot)
	set.seed(12)
	for (i in 1:num.boot) {
		boot.cox.d <- cox.d[sample(1:nrow(cox.d),replace=TRUE),]
		coxphf.obj <- coxphf(multi.formula.os,data=boot.cox.d)
		boot.hrs.os[i] <- coxphf.obj$coefficients["POLE.mut.consolidated.numeric"][[1]]
		
		boot.cox.dss.d <- cox.dss.d[sample(1:nrow(cox.dss.d),replace=TRUE),]
		coxphf.obj <- coxphf(multi.formula.dss,data=boot.cox.dss.d)
		boot.hrs.dss[i] <- coxphf.obj$coefficients["POLE.mut.consolidated.numeric"][[1]]
		
		boot.cox.rfs.d <- cox.rfs.d[sample(1:nrow(cox.rfs.d),replace=TRUE),]
		coxphf.obj <- coxphf(multi.formula.rfs,data=boot.cox.rfs.d)
		boot.hrs.rfs[i] <- coxphf.obj$coefficients["POLE.mut.consolidated.numeric"][[1]]
	}

	# warning messages during the code execution ...
    # Warning messages:
    # 1: In fitter(X, Y, strats, offset, init, control, weights = weights,  :
    #   Loglik converged before variable  1 ; beta may be infinite. 
    # 2: In fitter(X, Y, strats, offset, init, control, weights = weights,  :
    #   Loglik converged before variable  1 ; beta may be infinite. 
    # 3: In fitter(X, Y, strats, offset, init, control, weights = weights,  :
    #   Loglik converged before variable  1 ; beta may be infinite. 
    # 4: In fitter(X, Y, strats, offset, init, control, weights = weights,  :
    #   Loglik converged before variable  1 ; beta may be infinite. 
    # 5: In fitter(X, Y, strats, offset, init, control, weights = weights,  :
    #   Loglik converged before variable  1 ; beta may be infinite. 
    # 6: In fitter(X, Y, strats, offset, init, control, weights = weights,  :
    #   Loglik converged before variable  1 ; beta may be infinite. 
    # 7: In fitter(X, Y, strats, offset, init, control, weights = weights,  :
    #   Loglik converged before variable  1 ; beta may be infinite. 
	
	# write data to file.
	write.table(
		cbind(
			boot.hrs.os =boot.hrs.os,
			boot.hrs.dss=boot.hrs.dss,
			boot.hrs.rfs=boot.hrs.rfs
		),
		file="data/boot_firth_multi_cox_hrs.txt",
		sep="\t",
		col.names=TRUE,
		row.names=FALSE
	)
	
	boot.hrs.os  <- sort(boot.hrs.os)
	boot.hrs.dss <- sort(boot.hrs.dss)
	boot.hrs.rfs <- sort(boot.hrs.rfs)
} else {
	boot.hrs.d   <- read.delim(file="data/boot_firth_multi_cox_hrs.txt",sep="\t",header=TRUE,as.is=TRUE)
	boot.hrs.os  <- sort(boot.hrs.d$boot.hrs.os)
	boot.hrs.dss <- sort(boot.hrs.d$boot.hrs.dss)
	boot.hrs.rfs <- sort(boot.hrs.d$boot.hrs.rfs)
}




