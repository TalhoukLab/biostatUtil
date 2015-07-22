# classifier paper - Remark #14 (association between marker and variables)
# 
# Author: samuelc
###############################################################################

item14.correlation.table <- biostatUtil::doCohortCharacteristics(emdb, CLASS.CHOSEN, "", marker.value.labels.tolower=FALSE,
	c("age.at.surgery","bmi","stage_b1v234","Tumour.Grade","Histological.Subtype_non_endo","LVSI","any.positive.nodes","stmn.ihc.value.label","l1cam.ihc.value.label","ER.ihc.value.label", "PR.ihc.value.label","clin.path.risk.group","any.init.treatment"), 
	c(TRUE,            TRUE, FALSE,         FALSE,         FALSE,                          FALSE, FALSE,               FALSE,                 FALSE,                  FALSE,               FALSE,               FALSE,                 FALSE               ), 
	c(
		"Age at surgery (continuous)",
		"BMI (continuous)",
		"Stage",
		"Grade",
		"Histological subtype",
		"Lymphovascular invasion",
		"Any positive nodes",
		"stathmin (IHC)",
		"L1CAM (IHC)",
		"ER (IHC)",
		"PR (IHC)",
		"Clinical risk group",
		"Any initial adjuvant treatment"
	),
	stat.tests=c("kruskal","kruskal","fisher","fisher","fisher","fisher","fisher","fisher","fisher","fisher","fisher","fisher","fisher"),
	caption="Relation between classifier categories and patient characteristics",
	banded.rows=TRUE
)



