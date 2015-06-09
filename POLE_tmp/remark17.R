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


