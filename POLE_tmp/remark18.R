# POLE paper - Remark #18 (further investigation)
# e.g. check assumption 
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


# check proportional hazard of POLE
var.name <- "POLE.mut.germline.as.missing"
do.check.ph <- function(var.name,var.description) {

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


# sensitivity analysis per meeting with Aline 2015-05-27
# - want to check if diagnosis year is related to clincopathological parameters
#
# group yrs in to decades ...
item18.dx.year.correlation.table <- doCohortCharacteristics(emdb, "dx.decade", "Year", 
	c("POLE.mut.germline.as.missing","age.at.surgery","bmi","stage_b1v234","Tumour.Grade_b12v3","Histological.Subtype_non_endo","stage_x_endo","LVSI","any.positive.nodes","any.init.treatment"), 
	c(FALSE,TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE), 
	c(
		"POLE",
		"Age at surgery (continuous)",
		"BMI (continuous)",
		"Stage",
		"Grade",
		"Histological subtype",
		"Stage x histology",
		"Lymphovascular invasion",
		"Any positive nodes",
		"Any initial adjuvant treatment"
	),
	stat.tests=c("chisq","spearman","spearman","kendall","kendall","chisq","chisq","kendall","kendall","chisq"),
	caption="Relation between year of diagnosis and patient characteristics",
	banded.rows=TRUE
)



