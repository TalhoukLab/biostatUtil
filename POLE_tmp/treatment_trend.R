# look at treatment trend in endometrial database
# per meeting with Aline on 2015-05-25 
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

# generate a bar plot of treatment per dx years ...
do.treatment.trend.barplot <- function(input.d,title,legend.space=0.2) {
	legend.space <- round(legend.space*nrow(input.d))
	treatment.trend <- c()
	treatment.trend.n <- c()
	temp.d <- input.d
	dx.years <- names(table(temp.d$dx.year))
	use.prop <- TRUE
	for (dx.year in dx.years) {
		select.cases <- temp.d$dx.year==dx.year
		n <- sum(!temp.d$init.treatment[select.cases]%in%ALL.MISSING.CODES)
		num.chemo.only <- sum(temp.d$init.treatment[select.cases] ==   "chemo.only")	
		num.rt.only    <- sum(temp.d$init.treatment[select.cases] ==   "rt.only")
		num.chemo.rt   <- sum(temp.d$init.treatment[select.cases] ==   "both")
		num.no.tx      <- sum(temp.d$init.treatment[select.cases] %in% c("no.treatment","vag.brachy.only"))
		treatment <- c(num.chemo.only, num.rt.only, num.chemo.rt, num.no.tx)
		if (use.prop) {
			treatment <- treatment/n*100
		}
		treatment.trend <- cbind(treatment.trend,treatment)
		treatment.trend.n <- c(treatment.trend.n,n)
	}
	colnames(treatment.trend) <- dx.years
	if (length(dx.years)>20) {
		colnames(treatment.trend)[which(rep(c(FALSE,TRUE),11))] <- ""
	}
	if (sum(dx.years==2000)>0) {
		colnames(treatment.trend)[dx.years==2000] <- 2000
	}
	if (sum(dx.years==1999)>0) {
		colnames(treatment.trend)[dx.years==1999] <- ""
	}
	if (sum(dx.years==2001)>0) {
		colnames(treatment.trend)[dx.years==2001] <- ""
	}
	rownames(treatment.trend) <- c("chemo only","rt only","both","no treatment or vag brachy only")
	
	# add dummy bar for legend
	treatment.trend <- cbind(treatment.trend,rep("",nrow(treatment.trend)))
	colnames(treatment.trend)[ncol(treatment.trend)] <- ""
	treatment.trend.n <- c(treatment.trend.n,legend.space)
	
	barplot(
		treatment.trend, width=treatment.trend.n, 
		legend.text=T, args.legend = list(x="right"),col=c("red","yellow","brown","blue"),
		las=2,
		main=title,
		ylab="Percentage of cases",
		xlab="Year of diagnosis",
		sub="(width of bar proportional to the number of patients)"
	)
}

do.survival.trend.barplot <- function(input.d,title, surv.yrs, surv.sts, surv.event, event.desc, censor.desc, obs.yrs=3, legend.space=0.2) {
	legend.space <- round(legend.space*nrow(input.d))
	survival.trend <- c()
	survival.trend.n <- c()
	temp.d <- input.d
	dx.years <- names(table(temp.d$dx.year))
	use.prop <- TRUE
	for (dx.year in dx.years) {
		select.cases <- temp.d$dx.year==dx.year
		n <- sum(!is.na(temp.d[select.cases,surv.yrs]))
		event <- sum(temp.d[select.cases,surv.sts]==surv.event & as.numeric(temp.d[select.cases,surv.yrs])<=obs.yrs,na.rm=TRUE)
		censor <- n-event
		
		survival <- c(event,censor)
		if (use.prop) {
			survival <- survival/n*100
		}
		survival.trend <- cbind(survival.trend,survival)
		survival.trend.n <- c(survival.trend.n,n)
	}
	colnames(survival.trend) <- dx.years
	if (length(dx.years)>20) {
		colnames(survival.trend)[which(rep(c(FALSE,TRUE),11))] <- ""
	}
	if (sum(dx.years==2000)>0) {
		colnames(survival.trend)[dx.years==2000] <- 2000
	}
	if (sum(dx.years==1999)>0) {
		colnames(survival.trend)[dx.years==1999] <- ""
	}
	if (sum(dx.years==2001)>0) {
		colnames(survival.trend)[dx.years==2001] <- ""
	}
	rownames(survival.trend) <- c(event.desc, censor.desc)
	
	# add dummy bar for legend
	survival.trend <- cbind(survival.trend,rep("",nrow(survival.trend)))
	colnames(survival.trend)[ncol(survival.trend)] <- ""
	survival.trend.n <- c(survival.trend.n,legend.space)
	
	barplot(
			survival.trend, width=survival.trend.n, 
			legend.text=T, args.legend = list(x="right"),col=c("grey","pink"),
			las=2,
			main=title,
			ylab="Percentage of cases",
			xlab="Year of diagnosis",
			sub="(width of bar proportional to the number of patients)"
	)
}

do.grade.trend.barplot <- function(input.d,title,legend.space=0.2) {
	legend.space <- round(legend.space*nrow(input.d))
	grade.trend <- c()
	grade.trend.n <- c()
	temp.d <- input.d
	dx.years <- names(table(temp.d$dx.year))
	use.prop <- TRUE
	for (dx.year in dx.years) {
		select.cases <- temp.d$dx.year==dx.year
		n <- length(temp.d$Tumour.Grade[select.cases]) # no missing
		grade1 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 1")	
		grade2 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 2")
		grade3 <- sum(temp.d$Tumour.Grade[select.cases] == "Grade 3")
		grade <- c(grade1, grade2, grade3)
		if (use.prop) {
			grade <- grade/n*100
		}
		grade.trend <- cbind(grade.trend,grade)
		grade.trend.n <- c(grade.trend.n,n)
	}
	colnames(grade.trend) <- dx.years
	if (length(dx.years)>20) {
		colnames(grade.trend)[which(rep(c(FALSE,TRUE),11))] <- ""
	}
	if (sum(dx.years==2000)>0) {
		colnames(grade.trend)[dx.years==2000] <- 2000
	}
	if (sum(dx.years==1999)>0) {
		colnames(grade.trend)[dx.years==1999] <- ""
	}
	if (sum(dx.years==2001)>0) {
		colnames(grade.trend)[dx.years==2001] <- ""
	}
	rownames(grade.trend) <- c("Grade 1","Grade 2","Grade 3")
	
	# add dummy bar for legend
	grade.trend <- cbind(grade.trend,rep("",nrow(grade.trend)))
	colnames(grade.trend)[ncol(grade.trend)] <- ""
	grade.trend.n <- c(grade.trend.n,legend.space)
	
	barplot(
			grade.trend, width=grade.trend.n, 
			legend.text=T, args.legend = list(x="right"),col=c("red4","red3","red"),
			las=2,
			main=title,
			ylab="Percentage of cases",
			xlab="Year of diagnosis",
			sub="(width of bar proportional to the number of patients)"
	)
}
