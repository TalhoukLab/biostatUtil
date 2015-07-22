# Classifier paper - Remark #15 (univariable survival analysis)
# 
# Author: samuelc
###############################################################################

#coxph(Surv(emdb$os.yrs, emdb$os.sts=="os.event") ~ factor(emdb[,CLASS.CHOSEN],levels=CLASS.ORDER.CNLOW.AS.REF))
# Age
# BMI
# Stage (Will group togethers for all stage I, stage II etc-not substages)
# Grade
# Histol
# LVSI
# any positive nodes
# any adjuvant treatment
uni.cox.summary.all.variables <- biostatUtil::doCoxphGeneric( # a table of univariable cox models of POLE and other variables
	emdb,
	c(CLASS.CHOSEN,
		"age.at.surgery","bmi","stage_b1v234","Tumour.Grade","Histological.Subtype_non_endo","LVSI","any.positive.nodes","clin.path.risk.group",
		"stmn.ihc.consolidated.numeric","l1cam.ihc.consolidated.numeric","ER.ihc.consolidated.numeric","PR.ihc.consolidated.numeric",
		"any.init.treatment"), 
	c(
		paste(
			CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2],":<br><br><i>",
			CLASS.NAME.POLE,"<br>",
			CLASS.NAME.MSI,"<br>",
			CLASS.NAME.CNHIGH,"<br><br>",
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
		paste(CLIN.RISK.GROUP.NAME,"<br>continuous"),
		"stathmin (IHC)<br>continuous",
		"L1CAM (IHC)<br>continuous",
		"ER (IHC)",
		"PR (IHC)",
		"Any initial adjuvant treatment"),
	use.firth=FIRTH.THRESHOLD,
	stat.test="logtest",
	var.ref.groups=c(CLASS.NAME.CNLOW,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA),
	caption=paste("Univariable analyses of relation of classifier model (",CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2],") and standard variables to OS/DSS/RFS in whole cohort",sep=""),
	banded.rows=TRUE
)



