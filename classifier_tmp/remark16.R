# Classifier paper - Remark #16 (multivariable survival analysis)
# 
# Author: samuelc
###############################################################################

multi.cox.summary.clin.risk.group.only <- biostatUtil::doCoxphMultivariable( # a table of univariable cox models of POLE and other variables
	emdb,
	c(CLASS.CHOSEN,"clin.path.risk.group"), # per meeting with Aline 2015-01-23, remove treatment from mva
	c(
		paste(
			CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2],":<br><i>",
			CLASS.NAME.POLE,"<br>",
			CLASS.NAME.MSI,"<br>",
			CLASS.NAME.CNHIGH,"<br>",
			"(reference group: ",CLASS.NAME.CNLOW,")</i>",
			sep=""
		),
		"Clinical risk group"),
	use.firth=FIRTH.THRESHOLD,
	#firth.caption="", # use default ... whatever defined by FIRTH.CAPTION
	stat.test="logtest",
	var.ref.groups=c(CLASS.NAME.CNLOW,NA),
	caption=paste("Multivariable analyses of classifier model (",CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2],") with clinical risk group",sep=""),
	banded.rows=TRUE
)



#coxph(Surv(emdb$os.yrs, emdb$os.sts=="os.event") ~ factor(emdb[,CLASS.CHOSEN],levels=CLASS.ORDER.CNLOW.AS.REF))
# Age
# BMI
# Stage (Will group togethers for all stage I, stage II etc-not substages)
# Grade
# Histol
# LVSI
# any positive nodes
# any adjuvant treatment
temp.d <- emdb; temp.d$Tumour.Grade <- as.numeric(temp.d$Tumour.Grade) # want grade to be continuous instead of factor
multi.cox.summary.all.variables <- biostatUtil::doCoxphMultivariable( # a table of univariable cox models of POLE and other variables
	temp.d,
	c(
		CLASS.CHOSEN,"age.at.surgery","bmi","stage_b1v234","Tumour.Grade","Histological.Subtype_non_endo","LVSI","any.positive.nodes"	
	), # per meeting with Aline 2015-01-23, remove treatment from mva
	c(
		paste(
			CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2],":<br><i>",
			CLASS.NAME.POLE,"<br>",
			CLASS.NAME.MSI,"<br>",
			CLASS.NAME.CNHIGH,"<br>",
			"(reference group: ",CLASS.NAME.CNLOW,")</i>",
			sep=""
		),
		"Age at surgery<br>continuous",
		"BMI<br>continuous",
		"Stage<br>I vs. {II, III or IV}",
		"Grade<br>continuous",
		"Histological subtype<br>endometrioid vs. non-endometrioid",
		"Lymphovascular invasion",
		"Any positive nodes"
	),
	use.firth=FIRTH.THRESHOLD,
	#firth.caption="", # use default ... whatever defined by FIRTH.CAPTION
	stat.test="logtest",
	var.ref.groups=c(CLASS.NAME.CNLOW,NA,NA,NA,NA,NA,NA,NA),
	caption=paste("Multivariable analyses of classifier model (",CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2],") with standard variables",sep=""),
	banded.rows=TRUE
)
multi.cox.summary.all.variables.w.stmn.l1cam <- biostatUtil::doCoxphMultivariable( # a table of univariable cox models of POLE and other variables
	temp.d,
	c(
		CLASS.CHOSEN,"age.at.surgery","bmi","stage_b1v234","Tumour.Grade","Histological.Subtype_non_endo","LVSI","any.positive.nodes",
		"stmn.ihc.consolidated.numeric","l1cam.ihc.consolidated.numeric","PR.ihc.consolidated.numeric"	
	), # per meeting with Aline 2015-01-23, remove treatment from mva
	c(
		paste(
			CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2],":<br><i>",
			CLASS.NAME.POLE,"<br>",
			CLASS.NAME.MSI,"<br>",
			CLASS.NAME.CNHIGH,"<br>",
			"(reference group: ",CLASS.NAME.CNLOW,")</i>",
			sep=""
		),
		"Age at surgery<br>continuous",
		"BMI<br>continuous",
		"Stage<br>I vs. {II, III or IV}",
		"Grade<br>continuous",
		"Histological subtype<br>endometrioid vs. non-endometrioid",
		"Lymphovascular invasion",
		"Any positive nodes",
		"stathmin (IHC)<br>continuous",
		"L1CAM (IHC)<br>continuous",
		"PR (IHC)<br>negative vs. positive"
	),
	use.firth=FIRTH.THRESHOLD,
	#firth.caption="", # use default ... whatever defined by FIRTH.CAPTION
	stat.test="logtest",
	var.ref.groups=c(CLASS.NAME.CNLOW,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
	caption=paste("Multivariable analyses of classifier model (",CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2],") with standard variables",sep=""),
	banded.rows=TRUE
)

###############################################
### comparison of markers of CNHigh/CNLow   ###
###############################################

# per email from Jessica 2015-01-26, for comparison with FISH and p53, want to 
# restrict to case with interpretable FISH and p53 only

cn.d <- emdb[!is.na(emdb$CN1) & !is.na(emdb$CN2) & emdb[,CLASS.CHOSEN] %in% c(CLASS.NAME.CNHIGH, CLASS.NAME.CNLOW),]

# TODO: stat.tests should be as follow (currently function does NOT work) 
#    stat.tests=c("confusionMarkerAsRef","confusionMarkerAsRef","confusionMarkerAsRef")
item16.p53mut.vs.ihc.vs.fish.whole <- biostatUtil::doCohortCharacteristics(emdb[!is.na(emdb$CN1) & !is.na(emdb$CN2),], "TP53.mut.value.label", "P53 mutation", marker.value.labels.tolower=FALSE,
	c("CN1.value.label","CN2.value.label","p53.ihc.0vs12.value.label"), 
	c(FALSE,FALSE,FALSE), 
	c("FISH threshold 1","FISH threshold 2","p53 IHC"),
	stat.tests=c("chisq","chisq","chisq"),
	caption="p53 mutation status vs. FISH and p53 IHC - whole cohort (cases with interpretable FISH and p53 IHC only)",
	banded.rows=TRUE
)

# TODO: stat.tests should be as follow (currently function does NOT work) 
#    stat.tests=c("confusionMarkerAsRef","confusionMarkerAsRef","confusionMarkerAsRef")
item16.p53mut.vs.ihc.vs.fish.cn <- biostatUtil::doCohortCharacteristics(cn.d, "TP53.mut.value.label", "P53 mutation", marker.value.labels.tolower=FALSE,
	c("CN1.value.label","CN2.value.label","p53.ihc.0vs12.value.label"), 
	c(FALSE,FALSE,FALSE), 
	c("FISH threshold 1","FISH threshold 2","p53 IHC"),
	stat.tests=c("chisq","chisq","chisq"),
	caption="p53 mutation status vs. FISH and p53 IHC - within CNHigh/CNLow cases (cases with interpretable FISH and p53 IHC only)",
	banded.rows=TRUE
)

# KM plot of 

