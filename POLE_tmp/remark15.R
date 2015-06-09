# POLE paper - Remark #15 (univariable survival analysis)
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

# helper function to do km plot and coxph of all endpoints
# 
do.km.plots <- function(
		input.d, 
		var.name, 
		var.description,
		line.color=NULL,
		line.pattern=NULL,
		km.plot.ref.group="single", # specify KM plot reference group, "single" means a lump log-rank statistic 
		single.test.type="logrank",
		surv.type="os", # end point: os, dss, rfs
		use.firth=FIRTH.THRESHOLD,
		use.aline.plot=FALSE, # use Aline's plot function
		...
) {
	if (is.factor(input.d[,var.name])) {
		input.d[,var.name] <- droplevels(input.d[,var.name])
	}
	temp.d <- input.d
	temp.d$os.yrs <- as.numeric(temp.d$os.yrs)
	if (is.null(line.color)) {
		line.color <- c(1:length(names(table(temp.d[,var.name]))))
	}
	if (is.null(line.pattern)) {
		line.pattern <- 1
	}
	if (surv.type=="os") {
		formula.obj <- as.formula(paste("Surv(os.yrs,os.sts=='os.event') ~",var.name))
		sfit <- survfit(formula.obj, data=temp.d)
		# set local variable in environment searchable by local function calls
		assign("formula.obj",formula.obj, pos=1) 
		assign("temp.d",temp.d, pos=1) 
		
		if (!use.aline.plot) {
			plot_km(temp.d,
				formula.obj, #as.formula(paste("Surv(os.yrs,os.sts=='os.event') ~",var.name)),
				paste(var.description," (OS)",sep=""),
				OS.XLAB, # x-axis label
				OS.YLAB, # y-axis label
				names(table(temp.d[,var.name])), # line names
				line.color,
				line.pattern=line.pattern,
				show.test=km.plot.ref.group,   # show single or the reference group value (for pairwise comparison)
				# none = no test shown
				single.test.type=single.test.type, # the test to show if specified show.test="single". 
				# the possible choices are logrank, wilcoxon, taroneware, all
				obs.survyrs=3,
				...
			)
		} else {	
			ggkm(sfit,sfit2=NULL,
				table = TRUE,
				returns = FALSE,
				marks=TRUE,
				cols=rainbow(100),
				xlabs = OS.XLAB, #"Time",
				ylabs = OS.YLAB, #"Survival Probability",
				xlims = c(0,max(sfit$time)),
				ylims = c(0,1),
				ystratalabs = names(table(temp.d[,var.name])),#NULL,
				ystrataname = NULL,
				timeby = 1,
				main = paste(var.description," (OS)",sep=""),#"Kaplan-Meier Plot",
				pval = TRUE,
				HR=TRUE,
				use.firth=FIRTH.THRESHOLD,
				CI=TRUE,
				subs = NULL,
				legend=FALSE)	
		}
	} else if (surv.type=="dss") {
		temp.d <- input.d[!is.na(input.d$dss.sts),]
		temp.d$dss.yrs <- as.numeric(temp.d$dss.yrs)
		
		formula.obj <- as.formula(paste("Surv(dss.yrs,dss.sts=='dss.event') ~",var.name))
		sfit <- survfit(formula.obj, data=temp.d)
		# set local variable in environment searchable by local function calls
		assign("formula.obj",formula.obj, pos=1) 
		assign("temp.d",temp.d, pos=1) 
		
		if (!use.aline.plot) {
			dss.stats <- plot_km(temp.d,
				formula.obj, #as.formula(paste("Surv(dss.yrs,dss.sts=='dss.event') ~",var.name)),
				paste(var.description," (DSS)",sep=""),
				DSS.XLAB,
				DSS.YLAB,
				names(table(temp.d[,var.name])),
				line.color,
				line.pattern=line.pattern,
				show.test=km.plot.ref.group,   # show single or the reference group value (for pairwise comparison)
				# none = no test shown
				single.test.type=single.test.type, # the test to show if specified show.test="single". 
				# the possible choices are logrank, wilcoxon, taroneware, all
				obs.survyrs=3,
				... 
			)
		} else {
			ggkm(sfit,sfit2=NULL,
				table = TRUE,
				returns = FALSE,
				marks=TRUE,
				cols=rainbow(100),
				xlabs = DSS.XLAB, #"Time",
				ylabs = DSS.YLAB, #"Survival Probability",
				xlims = c(0,max(sfit$time)),
				ylims = c(0,1),
				ystratalabs = names(table(temp.d[,var.name])),#NULL,
				ystrataname = NULL,
				timeby = 1,
				main = paste(var.description," (DSS)",sep=""),#"Kaplan-Meier Plot",
				pval = TRUE,
				HR=TRUE,
				use.firth=FIRTH.THRESHOLD,
				CI=TRUE,
				subs = NULL,
				legend=FALSE)	
		}
	} else if (surv.type=="rfs") {
		temp.d <- input.d[!is.na(input.d$rfs.sts) & !is.na(input.d$rfs.yrs),]
		temp.d$rfs.yrs <- as.numeric(temp.d$rfs.yrs)
		
		formula.obj <- as.formula(paste("Surv(rfs.yrs,rfs.sts=='rfs.event') ~",var.name))
		sfit <- survfit(formula.obj, data=temp.d)
		# set local variable in environment searchable by local function calls
		assign("formula.obj",formula.obj, pos=1) 
		assign("temp.d",temp.d, pos=1) 
		
		if (!use.aline.plot) {
			rfs.stats <- plot_km(temp.d,
				formula.obj, #as.formula(paste("Surv(rfs.yrs,rfs.sts=='rfs.event') ~",var.name)),
				paste(var.description," (RFS)",sep=""),
				RFS.XLAB,
				RFS.YLAB,
				names(table(temp.d[,var.name])),
				line.color,
				line.pattern=line.pattern,
				show.test=km.plot.ref.group,   # show single or the reference group value (for pairwise comparison)
				# none = no test shown
				single.test.type=single.test.type, # the test to show if specified show.test="single". 
				# the possible choices are logrank, wilcoxon, taroneware, all
				obs.survyrs=3,
				... 
			)
		} else {
			ggkm(sfit,sfit2=NULL,
				table = TRUE,
				returns = FALSE,
				marks=TRUE,
				cols=rainbow(100),
				xlabs = RFS.XLAB, #"Time",
				ylabs = RFS.YLAB, #"Survival Probability",
				xlims = c(0,max(sfit$time)),
				ylims = c(0,1),
				ystratalabs = names(table(temp.d[,var.name])),#NULL,
				ystrataname = NULL,
				timeby = 1,
				main = paste(var.description," (RFS)",sep=""),#"Kaplan-Meier Plot",
				pval = TRUE,
				HR=TRUE,
				use.firth=FIRTH.THRESHOLD,
				CI=TRUE,
				subs = NULL,
				legend=FALSE)	
		}
	}
}


# univariable survival analyses of all variables are in POLE association tests
# Age
# BMI
# Stage (Will group togethers for all stage I, stage II etc-not substages)
# Grade
# Histol
# LVSI
# any positive nodes
# NO treatment
# RT only
# Chemo only
# Both
# Unknown treatment
# MMR IHC loss
# MMR IHC intact  (still deciding if we will share this in this paper)

# POLE
do.km.plots.pole <- function(
		surv.type,		
		use.aline.plot=FALSE # use Aline's plot function
){
	do.km.plots(
		emdb[!emdb$POLE.mut.germline.as.missing%in%ALL.MISSING.CODES,],
		"POLE.mut.germline.as.missing", 
		"POLE mutation status - whole cohort",
		single.test.type="logrank",
		surv.type=surv.type,
		conf.int=TRUE, # per missing 2015-01-09 ... wanted confidence interval on KM plots
		use.aline.plot=use.aline.plot
	)
	do.km.plots(
		emdb[!emdb$POLE.mut.germline.as.missing%in%ALL.MISSING.CODES & !emdb$init.treatment%in%ALL.MISSING.CODES & emdb$init.treatment=="no.treatment",],
		"POLE.mut.germline.as.missing", "POLE mutation status - no adjuvant treatment",
		single.test.type="logrank",
		surv.type=surv.type,
		conf.int=TRUE,
		use.aline.plot=use.aline.plot
	)
	do.km.plots(
		emdb[!emdb$POLE.mut.germline.as.missing%in%ALL.MISSING.CODES & !emdb$init.treatment%in%ALL.MISSING.CODES & emdb$init.treatment!="no.treatment",],
		"POLE.mut.germline.as.missing", "POLE mutation status - any adjuvant treatment",
		single.test.type="logrank",
		surv.type=surv.type,
		conf.int=TRUE,
		use.aline.plot=use.aline.plot
	)
}

do.km.plots.pole.x.init.treatment <- function(
		surv.type,
		use.aline.plot=FALSE # use Aline's plot function
) {
	do.km.plots(
		emdb[!emdb$POLE.mut.germline.as.missing%in%ALL.MISSING.CODES & !emdb$init.treatment%in%ALL.MISSING.CODES,],
		"POLE.x.init.treatment", "POLE mutation status x any adjuvant treatment",
		line.color=c("black","black","red","red"),
		line.width=c(3,1,3,1),
		single.test.type="none",
		surv.type=surv.type, 
		conf.int=TRUE,
		use.aline.plot=use.aline.plot
	)
}



# Age
# BMI
# Stage (Will group togethers for all stage I, stage II etc-not substages)
# Grade
# Histol
# LVSI
# any positive nodes
# NO treatment
# RT only
# Chemo only
# Both
# Unknown treatment
# MMR IHC loss
# MMR IHC intact  (still deciding if we will share this in this paper)
uni.cox.summary.all.variables <- do.coxph.generic( # a table of univariable cox models of POLE and other variables
		emdb, 
		c("POLE.mut.germline.as.missing","age.at.surgery","bmi","stage_b1v234","Tumour.Grade_b12v3","Histological.Subtype_non_endo","LVSI","any.positive.nodes","any.init.treatment"), 
		c(
			HTML.COX.VAR.DESC.POLE,
			HTML.COX.VAR.DESC.AGE,
			"BMI<br>continuous",
			HTML.COX.VAR.DESC.STAGE,
			HTML.COX.VAR.DESC.GRADE,
			HTML.COX.VAR.DESC.HIST,
			HTML.COX.VAR.DESC.LVSI,
			HTML.COX.VAR.DESC.ANY.POS.NODES,
			HTML.COX.VAR.DESC.ANY.INIT.ADJ.TX
		),
		use.firth=FIRTH.THRESHOLD,
		stat.test="logtest",
		var.ref.groups=c(POLE.REF.GROUP,NA,NA,NA,NA,NA,NA,NA,NA),
		caption="Univariable analyses of relation of POLE mutation status and standard variables to OS/DSS/RFS in whole cohort",
		banded.rows=TRUE)

uni.cox.summary.all.variables.no.init.treatment <- do.coxph.generic( # a table of univariable cox models of POLE and other variables
		emdb[emdb$any.init.treatment=="no.treatment",], 
		c("POLE.mut.germline.as.missing"                 ),#,"age.at.surgery","bmi","stage","Tumour.Grade","LVSI","MMR.IHC"), 
		c("POLE mutation status<br>wild type=0/mutated=1"),#,"age at surgery","BMI","stage","grade",       "LVSI","MMR<br>intact=0/absent=1"),
		use.firth=FIRTH.THRESHOLD,
		stat.test="logtest",
		var.ref.groups=c(POLE.REF.GROUP),
		caption="Univariable analyses of relation of POLE mutation status and standard variables to OS/DSS/RFS in cases with no initial adjuvant treatment",
		banded.rows=FALSE)

uni.cox.summary.all.variables.any.init.treatment <- do.coxph.generic( # a table of univariable cox models of POLE and other variables
		emdb[emdb$any.init.treatment=="any.treatment",], 
		c("POLE.mut.germline.as.missing"                 ),#"age.at.surgery","bmi","stage","Tumour.Grade","LVSI","MMR.IHC"), 
		c("POLE mutation status<br>wild type=0/mutated=1"),#"age at surgery","BMI","stage","grade",       "LVSI","MMR<br>intact=0/absent=1"),
		use.firth=FIRTH.THRESHOLD,
		stat.test="logtest",
		var.ref.groups=c(POLE.REF.GROUP),
		caption="Univariable analyses of relation of POLE mutation status and standard variables to OS/DSS/RFS in cases with any initial adjuvant treatment",
		banded.rows=FALSE)

do.km.plots.init.treatment <- function(){
	return(do.km.plots(
		emdb[!emdb$init.treatment%in%ALL.MISSING.CODES,],
		"init.treatment", "initial adjuvant treatment",
		km.plot.ref.group=VALUE.CODING.INIT.TREATMENT.NO,
		do.cox.model=FALSE
	))
}
do.km.plots.any.init.treatment <- function(){
	return(do.km.plots(
		emdb[!emdb$init.treatment%in%ALL.MISSING.CODES,],
		"any.init.treatment", "initial adjuvant treatment",
		do.cox.model=FALSE
	))
}


# meeting 2015-01-09 ...
# Jessica/Aline wanted to know the patients in POLE mutated / no treatment 
# who were censored before 4 years
if (!RUN.IN.MARKDOWN) {
	temp.d <- emdb[!is.na(emdb$any.init.treatment=="no.treatment") & emdb$any.init.treatment=="no.treatment" & emdb$POLE.mut.germline.as.missing==VALUE.CODING.MUTATED & emdb$os.sts=="os.censor" & emdb$os.yrs<4,]
	cat("per meeting 2015-01-09, want to see cases with POLE mutated, no treatment, os censored before 4 years\n")
	cat(paste(temp.d$Study.Identifier,collapse=","))
	cat("\n")
	
}
# list of cases who are censored before 2 years
censored.bf.2.years.os  <- emdb[emdb$os.yrs<2  & emdb$os.sts == "os.censor",c("TMA","Study.Identifier","Date.of.Surgery.mm.dd.yyyy","POLE.mut.germline.as.missing")]
censored.bf.2.years.dss <- emdb[emdb$dss.yrs<2 & emdb$dss.sts=="dss.censor",c("TMA","Study.Identifier","Date.of.Surgery.mm.dd.yyyy","POLE.mut.germline.as.missing")]
censored.bf.2.years.rfs <- emdb[emdb$rfs.yrs<2 & emdb$rfs.sts=="rfs.censor",c("TMA","Study.Identifier","Date.of.Surgery.mm.dd.yyyy","POLE.mut.germline.as.missing")]

censored.bf.2.years.dss <- censored.bf.2.years.dss[!is.na(censored.bf.2.years.dss$Study.Identifier),]
censored.bf.2.years.rfs <- censored.bf.2.years.rfs[!is.na(censored.bf.2.years.rfs$Study.Identifier),]

censored.bf.2.years.os[ ,"POLE.mut.germline.as.missing"] <- sapply(censored.bf.2.years.os[ ,"POLE.mut.germline.as.missing"] ,as.character)
censored.bf.2.years.dss[,"POLE.mut.germline.as.missing"] <- sapply(censored.bf.2.years.dss[,"POLE.mut.germline.as.missing"] ,as.character)
censored.bf.2.years.rfs[,"POLE.mut.germline.as.missing"] <- sapply(censored.bf.2.years.rfs[,"POLE.mut.germline.as.missing"] ,as.character)

names(censored.bf.2.years.os ) <- c("TMA","Study ID","Date of Surgery","POLE")
names(censored.bf.2.years.dss) <- c("TMA","Study ID","Date of Surgery","POLE")
names(censored.bf.2.years.rfs) <- c("TMA","Study ID","Date of Surgery","POLE")

rownames(censored.bf.2.years.os ) <- NULL
rownames(censored.bf.2.years.dss) <- NULL
rownames(censored.bf.2.years.rfs) <- NULL



