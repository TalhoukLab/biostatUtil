# POLE paper - Remark #14 (association between marker and variables)
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
	options("width" = 200) # more columns for easier viewing via console
}

item14.correlation.table <-
  doCohortCharacteristics(emdb, "POLE.mut.germline.as.missing", "POLE",
	c("age.at.surgery", "bmi", "stage_b1v234", "Tumour.Grade_b12v3",
	  "Histological.Subtype_non_endo", "LVSI", "any.positive.nodes",
	  "any.init.treatment"), 
	c(TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE), 
	c("Age at surgery (continuous)", "BMI (continuous)", "Stage", "Grade",
		"Histological subtype", "Lymphovascular invasion", "Any positive nodes",
		"Any initial adjuvant treatment"),
	stat.tests = c("spearman", "spearman", "kendall", "kendall",
	               "chisq", "kendall", "kendall", "chisq"),
	caption = "Relation between POLE mutation status and patient characteristics",
	banded.rows = TRUE)

# per meeting with Aline 2015-05-25, want to do logistic regression ... POLE ~ clinicopath parameters
# logit(POLE) ~ age+bmi+stage+grade+hist+LVSI+nodes+tx
logit.ex.bmi.d <- emdb[ # no missing POLE, 
	emdb$age.at.surgery_bXvS == VALUE.CODING.AVAILABLE & 
	emdb$stage_bXvS == VALUE.CODING.AVAILABLE &
	emdb$Tumour.Grade_bXvS == VALUE.CODING.AVAILABLE &
	emdb$Histological.Subtype_bXvS == VALUE.CODING.AVAILABLE &
	emdb$LVSI_bXvS == VALUE.CODING.AVAILABLE &
	emdb$any.positive.nodes_bXvS == VALUE.CODING.AVAILABLE &
	emdb$any.init.treatment_bXvS == VALUE.CODING.AVAILABLE, ]

logit.ex.bmi.d$stage_b1v234 <- sapply(logit.ex.bmi.d$stage_b1v234,
                                      as.character)
logit.ex.bmi.d$stage_b1v234 <- 
  factor(logit.ex.bmi.d$stage_b1v234,
         levels = names(table(logit.ex.bmi.d$stage_b1v234)))
logit.ex.bmi.d$LVSI <- sapply(logit.ex.bmi.d$LVSI, as.character)
logit.ex.bmi.d$LVSI <-
  factor(logit.ex.bmi.d$LVSI,
         levels = names(table(logit.ex.bmi.d$LVSI)))
logit.ex.bmi.d$any.positive.nodes <-
  sapply(logit.ex.bmi.d$any.positive.nodes, as.character)
logit.ex.bmi.d$any.positive.nodes <-
  factor(logit.ex.bmi.d$any.positive.nodes,
         levels = names(table(logit.ex.bmi.d$any.positive.nodes)))

logit.d <- logit.ex.bmi.d[logit.ex.bmi.d$bmi_bXvS == VALUE.CODING.AVAILABLE, ]

# NOTE: logistf(), response must be numeric or boolean
logit.full       <-glm(            POLE.mut.consolidated.numeric~age.at.surgery+bmi+stage_b1v234+Tumour.Grade_b12v3+Histological.Subtype_non_endo+LVSI+any.positive.nodes+any.init.treatment,data=logit.d,       family=binomial("logit"))
logit.full.firth <-logistf(        POLE.mut.consolidated.numeric~age.at.surgery+bmi+stage_b1v234+Tumour.Grade_b12v3+Histological.Subtype_non_endo+LVSI+any.positive.nodes+any.init.treatment,data=logit.d)

logit.full.ex.bmi       <-glm(     POLE.mut.consolidated.numeric~age.at.surgery    +stage_b1v234+Tumour.Grade_b12v3+Histological.Subtype_non_endo+LVSI+any.positive.nodes+any.init.treatment,data=logit.ex.bmi.d,family=binomial("logit"))
logit.full.ex.bmi.firth <- logistf(POLE.mut.consolidated.numeric~age.at.surgery    +stage_b1v234+Tumour.Grade_b12v3+Histological.Subtype_non_endo+LVSI+any.positive.nodes+any.init.treatment,data=logit.ex.bmi.d)

logit.full.reduced        <- step(logit.full,test="LRT")
logit.full.ex.bmi.reduced <- step(logit.full.ex.bmi,test="LRT")

# model selection function provided by logistf package
logit.full.firth.reduced        <- backward(logit.full.firth)
logit.full.ex.bmi.firth.reduced <- backward(logit.full.ex.bmi.firth)

# try backward manually using extractAIC() !!!
backward.by.aic <- function(logistf.fit) {
	temp.d <- logistf.fit$data
	curr.fit <- logistf.fit
	curr.formula <- logistf.fit$formula
	curr.aic <- extractAIC(logistf.fit)[2]

	# try reduced model ...
	for (term.to.exclude in logistf.fit$terms) {
		
	}	
}