# POLE paper - Remark #13 (cohort characteristics)
# 
# Author: samuelc
###############################################################################

### Distribution of classifier model with other variables ... raw categories when possible
# MMR/POLE/p53 mutation/p53 IHC
# Age
# BMI
# Stage (Will group togethers for all stage I, stage II etc-not substages)
# Grade
# Histol
# LVSI
# any positive nodes
# clin.path.risk.group
# initial adjuvant treatment

missing.codes.highlights <- list(
	"MMR.IHC.4"=NA,
	"POLE.mut.germline.as.missing"=NA,
	"TP53.mut.value.label"=NA,
	"p53.ihc.0vs12.value.label"=NA,
	"age.at.surgery"=NA,
	"bmi"=NA,
	"stage"=NA,
	"Tumour.Grade"=NA,
	"Histological.Subtype"=NA,
	"grade_x_endo"=NA,
	"LVSI"=NA,
	"any.positive.nodes.for.cohort.char"="No lymph node dissection performed",
	"clin.path.risk.group"=NA,
	"init.treatment"=NA
)

item13.cohort.characteristics <- biostatUtil::doCohortCharacteristics(emdb, CLASS.CHOSEN, "", marker.value.labels.tolower=FALSE,
	c(
		"MMR.IHC.4","POLE.mut.germline.as.missing","TP53.mut.value.label","p53.ihc.0vs12.value.label",
		"age.at.surgery","bmi","stage","Tumour.Grade","Histological.Subtype","grade_x_endo","LVSI","any.positive.nodes.for.cohort.char","clin.path.risk.group","init.treatment"
	), 
	c(
		FALSE,FALSE,FALSE,FALSE,
		TRUE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE,FALSE
	), 
	c(
		"MMR (IHC)","POLE mutation","p53 mutation","p53 (IHC)",
		"Age at surgery","BMI","Stage","Grade","Histological subtype","Grade x Histological subtype","Lymphovascular invasion","Any positive nodes","Clinical risk group","Initial adjuvant treatment"
	),
	missing.codes.highlight=missing.codes.highlights,
	caption=paste("Cohort characteristics:",CLASS.NAMES[CLASS.NAMES[,1]==CLASS.CHOSEN,2]),
	banded.rows=TRUE
)

# manually checked and there are no mising: age, stage, grade, histology, clinical risk group 
item13.cohort.missing <- biostatUtil::doCohortCharacteristics(emdb, CLASS.CHOSEN, "", marker.value.labels.tolower=FALSE,
	c("bmi_bXvS","LVSI_bXvS","any.positive.nodes_bXvS","any.init.treatment_bXvS"),
	c(FALSE,FALSE,FALSE,FALSE), 
	c("BMI","Lymphovascular invasion","Any positive nodes","Initial adjuvant treatment"),
	stat.tests=c("fisher","fisher","fisher","fisher"),
	stat.test.column.header="association test",
	do.droplevels=FALSE,
	caption="Missing value analysis",
	banded.rows=TRUE,
	show.missing=FALSE
)




