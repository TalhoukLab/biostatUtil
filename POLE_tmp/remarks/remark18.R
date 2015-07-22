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
	source('SetUp_db.R')
}

# sensitivity analysis per meeting with Aline 2015-05-27
# - want to check if diagnosis year is related to clincopathological parameters
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