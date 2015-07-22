# Database set up for classifier paper e.g. do censoring, exclude cases ... etc
# 
# also get library files
#
# Author: samuelc
###############################################################################
options(stringsAsFactors=TRUE) # NEED to be TRUE 
#options(table_counter_str="")

# get library files
library(knitr)
library(powerSurvEpi)
library(dplyr)
require(Gmisc)
library(survival)
library(cgdsr)
library(RColorBrewer)
library(ggplot2)
library(gridExtra)
library(blockcluster)
library(bccaEndometrial) # data package!
library(tcgaEndometrial) # data package!

data(ed)
data(tcgaEd)

### constants ###
MISSING.EXPLICIT <- "N/A" # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING.UNK <- "Unk" # missing because values was not found (e.g. in data files) but the value must exist somewhere.
MISSING...NOT.FOUND.IN.DATA.FILE <- "" # data point not mentioned in data file.
MISSING.BIOMARKER.EXPLICIT <- MISSING.UNK # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE <- "" # data point not mentioned in data file.
ALL.MISSING.CODES <- unique(c(MISSING.EXPLICIT, MISSING...NOT.FOUND.IN.DATA.FILE, MISSING.UNK, MISSING.BIOMARKER.EXPLICIT, MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE))

DATE.FORMAT="%m/%d/%Y" # this should be R's default date format
VALUE.CODING.WILD.TYPE <- "wild type"
VALUE.CODING.MUTATED <- "mutated"
VALUE.CODING.MMR.ABSENT <- "absent"
VALUE.CODING.MMR.INTACT <- "intact"
VALUE.CODING.MMR.MSI.HIGH <- "MSI-high"
VALUE.CODING.MMR.MSS <- "MSS"
VALUE.CODING.TCGA.MSI.HIGH <- "MSI-H"
VALUE.CODING.INIT.TREATMENT.NO <- "no.treatment"
VALUE.CODING.INIT.TREATMENT.CHEMO.ONLY <- "chemo.only"
VALUE.CODING.INIT.TREATMENT.RT.ONLY <- "rt.only"
VALUE.CODING.INIT.TREATMENT.VAG.BRACHY.ONLY <- "vag.brachy.only"
VALUE.CODING.INIT.TREATMENT.BOTH <- "both"
VALUE.CODING.INIT.TREATMENT.ANY <- "any.treatment"
VALUE.CODING.AVAILABLE <- "available"
VALUE.CODING.MISSING <- "missing"
VALUE.CODING.CLIN.RISK.GROUP.LOW <- "low"
VALUE.CODING.CLIN.RISK.GROUP.INT <- "intermediate"
VALUE.CODING.CLIN.RISK.GROUP.HIGH <- "high"
VALUE.CODING.FISH.NOT.AMPLIFIED <- "not amplified" 
VALUE.CODING.FISH.AMPLIFIED <- "copy number high"
VALUE.CODING.P53.IHC.1 <- "1+"
VALUE.CODING.P53.IHC.0.2 <- "0 or 2+"
VALUE.CODING.WILD.TYPE.NON.AMPLIFIED <- "wild type/non-amplified"
VALUE.CODING.WILD.TYPE.AMPLIFIED <- "wild type/amplified"
VALUE.CODING.MUTATED.NON.AMPLIFIED <- "mutated/non-amplified"
VALUE.CODING.MUTATED.AMPLIFIED <- "mutated/amplified"
VALUE.CODING.WILD.TYPE.1 <- "wild type/1+"
VALUE.CODING.WILD.TYPE.0.2 <- "wild type/0 or 2+"
VALUE.CODING.MUTATED.1 <- "mutated/1+"
VALUE.CODING.MUTATED.0.2 <- "mutated/0 or 2+"

FIRTH.THRESHOLD <- 0.8 # percent of censor cases to use Firth correction in Cox model
FIRTH.CAPTION <- "<sup>(F)</sup>" # html text to be placed beside the p-value to indicate that the Cox model used Firth

CLASS.NAME <- "TCGA-inspired endometrial subtypes" # not sure how to call this, therefore make it a variable and have the group decide
CLIN.RISK.GROUP.NAME <- "Clinical (ESMO) risk group"

NUM.DIGITS.TO.SHOW <- 1 # number of significant digits to show 

### end of constants ###

### READ DATA FILE ###
emdbAll=ed
emdb=emdbAll[emdbAll$TMA=="11-010 (VOA endometrial)", ] # n=157

# jessica email 2015-01-15 ... "Can you venture a number for next confirmation step in grant that will be done in our cohort for classifier (not classifier paper but proposed next step in grant)"
temp.d <- emdbAll[emdbAll$followup.lost != "Yes",]
FindDuplicates=grep("duplicate",temp.d[,"comment.about.duplicate"]) # Find Duplicates
temp.d <- temp.d[-FindDuplicates,]
FindNeoAdj= which(as.Date(temp.d[,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT)<as.Date(temp.d[,"Date.of.Surgery.mm.dd.yyyy"],format=DATE.FORMAT))
temp.d <- temp.d[-FindNeoAdj,]
# per jessica email, remove all POLE unk ... will not be able to recover these data for validation set ...
unk.pole <- temp.d$POLE.mut.consolidated.numeric=="Unk"
temp.d <- temp.d[!unk.pole & temp.d$POLE.mut.consolidated.numeric != "1 germline",]

# we need to start with n=152 that were used in McConechy et al. J. Path 2012 ...
# 1. remove 1 case that was excluded from previous studies (i.e. for J. Path 2012) due to this case being endometrial clear cell carcinoma: VOA923
#    this case does not have mutation data ... so it won't be used anyways
# 2. remove 4 cases were excluded due to various reason (e.g. not endometrial cancer): VOA614, VOA630, VOA1032, VOA1126
#    these are already remove before n=518
# 3. remove cases not in TMA but was part of the "11-010" cohort analyzed by Melissa's Fluidigm-MiSeq: VOA267, VOA694, VOA743, VOA961
#    these cases will not be used in the study since they do not have any IHC data (because they are not part of TMA)
emdb <- emdb[emdb$Study.Identifier!="VOA923" & 	!emdb$Study.Identifier %in% c("VOA267", "VOA694", "VOA743", "VOA961"),] 
emdbJPath <- emdb

# remove case with neoadjuvant treatment VOA1090
FindNeoAdj= which(as.Date(emdb[,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT)<as.Date(emdb[,"Date.of.Surgery.mm.dd.yyyy"],format=DATE.FORMAT))
emdb=emdb[-FindNeoAdj,] 

clinPathVars=c("TMA","Study.Identifier" ,"FIGO.Stage.2009", "Tumour.Grade","Histological.Subtype")
FishVars=c(
	"myc.fish.t1.consolidated.numeric" ,"myc.fish.t2.consolidated.numeric",
	"sox.fish.t1.consolidated.numeric","sox.fish.t2.consolidated.numeric",
	"fgfr.fish.t1.consolidated.numeric","fgfr.fish.t2.consolidated.numeric");#,"MMR.Status");
MutVars=names(emdb)[grep("mut.consolidated",names(emdb))]
IHCVars=names(emdb)[grep("ihc.consolidated",names(emdb))]
Allvars=c(clinPathVars,FishVars,MutVars,IHCVars)

### factor ...
#   change some text values to factor so that when we do tables or km plots, 
#   they would appear in a preferred order 
# 
# FISH (MYC, SOX and FGFR) - recode to copy number high/low bin variable
FishDat=emdb[,FishVars];FishDat[FishDat=="Unk"]=NA
#emdb$CN1=factor(ifelse(rowSums(data.matrix(FishDat[,grep("t1",names(FishDat))]),na.rm=T)>1,1,0))
#emdb$CN2=factor(ifelse(rowSums(data.matrix(FishDat[,grep("t2",names(FishDat))]),na.rm=T)>1,1,0))
emdb$CN1=apply(emdb[,FishVars[grep("t1",FishVars)]],1,function(x){
	x <- x[!is.na(x)]
	if (sum(x%in%ALL.MISSING.CODES)>1) { # missing if > 1 gene missing ... manually checked and all cases with 1 gene missing, both non-missing genes are not amplified, therefore, can safely say case is not amplified
		return(NA)
	} else {
		return(as.numeric(sum(as.numeric(x[!x%in%ALL.MISSING.CODES]),na.rm=TRUE)>1))
	}			
})
emdb$CN2=apply(emdb[,FishVars[grep("t2",FishVars)]],1,function(x){
	x <- x[!is.na(x)]
	if (sum(x%in%ALL.MISSING.CODES)>1) { # missing if > 1 gene missing ... manually checked and all cases with 1 gene missing, both non-missing genes are not amplified, therefore, can safely say case is not amplified
		return(NA)
	} else {
		return(as.numeric(sum(as.numeric(x[!x%in%ALL.MISSING.CODES]),na.rm=TRUE)>1))
	}			
})
emdb$CN1.value.label <- factor(sapply(emdb$CN1,function(x){return(ifelse(is.na(x),NA,ifelse(x==0,VALUE.CODING.FISH.NOT.AMPLIFIED,VALUE.CODING.FISH.AMPLIFIED)))}),levels=c(VALUE.CODING.FISH.NOT.AMPLIFIED,VALUE.CODING.FISH.AMPLIFIED))
emdb$CN2.value.label <- factor(sapply(emdb$CN2,function(x){return(ifelse(is.na(x),NA,ifelse(x==0,VALUE.CODING.FISH.NOT.AMPLIFIED,VALUE.CODING.FISH.AMPLIFIED)))}),levels=c(VALUE.CODING.FISH.NOT.AMPLIFIED,VALUE.CODING.FISH.AMPLIFIED))

# show number of cases with unknown fish ...
table(apply(emdb[,FishVars[grep("t1",FishVars)]],1,function(x){return(sum(is.na(x))+sum(x=="Unk",na.rm=TRUE)==3)}))

# POLE - lump missing with germline mutation
POLE.GERMLINE.FLAG <- "1 germline"
consolidate.pole.mv.germline.to.unk <- function(x){
	return (ifelse(
		x %in% c(ALL.MISSING.CODES,POLE.GERMLINE.FLAG), 
		MISSING.BIOMARKER.EXPLICIT, 
		ifelse(x==0,VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))
	)
}
emdb$POLE.mut.germline.as.missing             <- factor(sapply(            emdb$POLE.mut.consolidated.numeric,consolidate.pole.mv.germline.to.unk),levels=c(MISSING.BIOMARKER.EXPLICIT,VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))

# PTEN
emdb$PTEN.mut.value.label <- factor(sapply(emdb$PTEN.mut.consolidated.numeric,function(x){return(ifelse(x==0,VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))}),levels=c(VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))
# P53
emdb$TP53.mut.value.label <- factor(sapply(emdb$TP53.mut.consolidated.numeric,function(x){return(ifelse(x==0,VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))}),levels=c(VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))

# p53 IHC - 1 vs. 0,2
emdb$p53.ihc.0vs12.value.label <- factor(sapply(emdb$p53.ihc.consolidated.numeric,function(x){
	if (x%in%ALL.MISSING.CODES) {
		return(NA)
	} else {
		return(ifelse(x==1,VALUE.CODING.P53.IHC.1,VALUE.CODING.P53.IHC.0.2))
	}		
}), levels=c(VALUE.CODING.P53.IHC.1, VALUE.CODING.P53.IHC.0.2))

# P53 mut x FISH
assign.p53mut.x.fish <- function(x){
	if (sum(is.na(x))>0) {
		return(NA) 
	} else if (sum(x%in%ALL.MISSING.CODES)>0) {
		return(NA)
	} else { # both interpretable
		x <- as.numeric(x)
		if (x[1]==0 & x[2]==0) {
			return(VALUE.CODING.WILD.TYPE.NON.AMPLIFIED)
		} else if (x[1]==0 & x[2]==1) {
			return(VALUE.CODING.WILD.TYPE.AMPLIFIED)
		} else if (x[1]==1 & x[2]==0) {
			return(VALUE.CODING.MUTATED.NON.AMPLIFIED)
		} else {
			return(VALUE.CODING.MUTATED.AMPLIFIED)
		}
	}	
}
emdb$p53mut.x.CN1 <- apply(emdb[,c("TP53.mut.consolidated.numeric","CN1")],1,assign.p53mut.x.fish)
emdb$p53mut.x.CN2 <- apply(emdb[,c("TP53.mut.consolidated.numeric","CN2")],1,assign.p53mut.x.fish)

# P53 mut x IHC
emdb$p53mut.x.ihc <- apply(emdb[,c("TP53.mut.consolidated.numeric","p53.ihc.consolidated.numeric")],1, function(x){
	if (sum(is.na(x))>0) {
		return(NA) 
	} else if (sum(x%in%ALL.MISSING.CODES)>0) {
		return(NA)
	} else { # both interpretable
		x <- as.numeric(x)
		if (x[1]==0 & x[2]==1) {
			return(VALUE.CODING.WILD.TYPE.1)
		} else if (x[1]==0 & x[2]%in%c(0,2)) {
			return(VALUE.CODING.WILD.TYPE.0.2)
		} else if (x[1]==1 & x[2]==1) {
			return(VALUE.CODING.MUTATED.1)
		} else {
			return(VALUE.CODING.MUTATED.0.2)
		}
	}	
})

# treatment
emdb$init.treatment <- factor(emdb$init.treatment, 
	levels=c(
		MISSING...NOT.FOUND.IN.DATA.FILE, # assume this is the ONLY missing category!!!!
		VALUE.CODING.INIT.TREATMENT.NO,
		VALUE.CODING.INIT.TREATMENT.VAG.BRACHY.ONLY,
		VALUE.CODING.INIT.TREATMENT.RT.ONLY,
		VALUE.CODING.INIT.TREATMENT.CHEMO.ONLY,
		VALUE.CODING.INIT.TREATMENT.BOTH
))
emdb$any.init.treatment <- factor(sapply(emdb$init.treatment,function(x){
	x <- as.character(x)
	if (x %in% ALL.MISSING.CODES) {
		return(x)
	} else {
		return(ifelse(x!=VALUE.CODING.INIT.TREATMENT.NO,VALUE.CODING.INIT.TREATMENT.ANY,VALUE.CODING.INIT.TREATMENT.NO))
	}
}),levels=c(VALUE.CODING.INIT.TREATMENT.NO,VALUE.CODING.INIT.TREATMENT.ANY)) # since only two levels are specified, missing will turn into NA

emdb$any.init.treatment_bXvS <- sapply(is.na(emdb$any.init.treatment),function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))}) 

#
# POLE x treatment variable
# - exclude POLE germline
# - exclude unknown POLE or unknown treatment
emdb$POLE.x.init.treatment <- factor(apply(emdb[,c(
	"POLE.mut.germline.as.missing", #1
	"init.treatment"                #2
)],1,function(x){
	x <- as.character(x)
	if (sum(x %in% ALL.MISSING.CODES)>0 | x[1]==POLE.GERMLINE.FLAG) {
		return(NA)
	} else {
		if (x[1]=="wild type") {
			return(ifelse(x[2]=="no.treatment","wt/no.tx","wt/any.tx"))
		} else {
			return(ifelse(x[2]=="no.treatment","mut/no.tx","mut/any.tx"))
		}
	}
}),levels=c("wt/no.tx","wt/any.tx","mut/no.tx","mut/any.tx"))

# clinical risk group
emdb$clin.path.risk.group=factor(sapply(emdb$clin.path.risk.group,function(x){
	return(switch(as.character(x),
		"1"=VALUE.CODING.CLIN.RISK.GROUP.LOW,
		"2"=VALUE.CODING.CLIN.RISK.GROUP.INT,
		"3"=VALUE.CODING.CLIN.RISK.GROUP.HIGH
	))					
}),levels=c(VALUE.CODING.CLIN.RISK.GROUP.LOW, VALUE.CODING.CLIN.RISK.GROUP.INT, VALUE.CODING.CLIN.RISK.GROUP.HIGH))

### other formatting & calculations ...
#
# age - change to numeric
emdb$age.at.surgery <- sapply(emdb$age.at.surgery, function(x){return(ifelse(x==MISSING.EXPLICIT,NA,as.numeric(x)))})
emdb$age.at.surgery_bXvS <- sapply(is.na(emdb$age.at.surgery),function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))})

# bmi - weight(kg)/(height(m))^2
emdb$bmi <- 
	sapply(emdb$Weight,function(x){return(ifelse(x %in% ALL.MISSING.CODES,NA,as.numeric(x)))})/
	sapply(emdb$Height,function(x){return(ifelse(x %in% ALL.MISSING.CODES,NA,(as.numeric(x)/100)^2))})
emdb$bmi_bXvS <- sapply(is.na(emdb$bmi),function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))})

# stage - group togethers for all stage I, stage II etc-not substages
emdb$stage <-
	factor(sapply(emdb$FIGO.Stage.2009,function(x){
		if (x %in% ALL.MISSING.CODES) {
			return(x) # preserve missing codes
		} else if (biostatUtil::startsWith(x,"IV")){
			return("IV")
		} else if (biostatUtil::startsWith(x,"III")) {
			return("III")
		} else if (biostatUtil::startsWith(x,"II")) {
			return("II")
		} else if (biostatUtil::startsWith(x,"I")) {
			return("I")
		} else {
			return(NA) # should not happen!!!
		}
	}),levels=c(MISSING.EXPLICIT, "I", "II", "III", "IV"))
emdb$stage_b1v234 <- factor(sapply(emdb$stage,function(x){
		x <- as.character(x)
		if (x %in% ALL.MISSING.CODES) {
			return(x) # preserve missing codes
		} else {
			return(ifelse(x=="I","I","II, III or IV"))
		}
	}),levels=c(MISSING.EXPLICIT, "I","II, III or IV"))
emdb$stage_bXvS <- sapply(emdb$stage %in% ALL.MISSING.CODES,function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))})

# Tumour.Grade - change to factor
#emdb$Tumour.Grade <- factor(emdb$Tumour.Grade, levels=names(table(emdb$Tumour.Grade)))
emdb$Tumour.Grade <- factor(sapply(emdb$Tumour.Grade, function(x){
	return(ifelse(x=="Grade 2/3","Grade 3",x))
}),levels=c("Grade 1","Grade 2","Grade 3"))
emdb$Tumour.Grade_b12v3 <- factor(sapply(emdb$Tumour.Grade,function(x){
		x <- as.character(x)
		return(ifelse(x %in% c("Grade 2/3","Grade 3"),"Grade 3","Grade 1 or 2"))
	}),levels=c("Grade 1 or 2","Grade 3"))
emdb$Tumour.Grade_bXvS <- factor(sapply(emdb$Tumour.Grade %in% ALL.MISSING.CODES,function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))}),levels=c(VALUE.CODING.AVAILABLE,VALUE.CODING.MISSING)) 

# Histology
histological.subtype.levels <- c(
	"Endometrioid",
	"Serous",
	#"Clear Cell", # 
	#"Small Cell", # because we are only look at 11-010, there are no "Small Cell" in 11-010
	"Mixed Endometrioid And Clear Cell",
	"Mixed Endometrioid With Undifferentiated", # remove this if removing unknown p53 IHC
	"Mixed Serous And Carcinosarcoma",
	"Mixed Serous And Clear Cell",
	"Mixed Serous And Endometrioid",
	"Mixed Serous And Low-Grade Endometrioid",
	"Undifferentiated" 
)
if (sum(names(table(emdb$Histological.Subtype)) %in% histological.subtype.levels) != length(histological.subtype.levels) | length(names(table(emdb$Histological.Subtype))) != length(histological.subtype.levels)) {
	cat("ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! \n")
	cat("ERROR!!! histological subtype changed ... factors NO LONGER VALIDE ... PLEASE CHECK Setup_db.R!!!!!\n")
	cat("ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! ERROR!!! \n")
}
emdb$Histological.Subtype <- factor(emdb$Histological.Subtype, levels=histological.subtype.levels)
emdb$Histological.Subtype_non_endo <- factor(sapply(emdb$Histological.Subtype,function(x){
	x <- as.character(x)
	return(ifelse(x=="Endometrioid","Endometrioid","Non-Endometrioid"))
}),levels=c("Endometrioid","Non-Endometrioid"))
emdb$Histological.Subtype_bXvS <- factor(sapply(emdb$Histological.Subtype %in% ALL.MISSING.CODES,function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))}),levels=c(VALUE.CODING.AVAILABLE,VALUE.CODING.MISSING)) 

# Grade {1/2 vs. 3} x Histology {endo vs. non-endo}
emdb$grade_x_endo <- paste(emdb$Tumour.Grade_b12v3, "/", emdb$Histological.Subtype_non_endo)
emdb$grade_x_endo <- factor(emdb$grade_x_endo,levels=names(table(emdb$grade_x_endo)))

# LVSI 
emdb$LVSI <- factor(emdb$LVSI,levels=names(table(emdb$LVSI)))
emdb$LVSI_bXvS <- sapply(emdb$LVSI %in% ALL.MISSING.CODES,function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))}) 

# any positive nodes
emdb$any.positive.nodes.for.cohort.char <- emdb$any.positive.nodes # this variable includes the "no dissection done category"
emdb$any.positive.nodes.for.cohort.char[emdb$any.positive.nodes.for.cohort.char=="No"] <- "Lymph node dissection performed:<br>&nbsp;&nbsp;&nbsp;(no positive nodes found)"
emdb$any.positive.nodes.for.cohort.char[emdb$any.positive.nodes.for.cohort.char=="Yes"] <- "Lymph node dissection performed:<br>&nbsp;&nbsp;&nbsp;(positive nodes found)"
emdb$any.positive.nodes.for.cohort.char[emdb$any.positive.nodes.for.cohort.char=="No dissection done"] <- "No lymph node dissection performed"
emdb$any.positive.nodes.for.cohort.char <- factor(
		emdb$any.positive.nodes.for.cohort.char ,
		levels=c("Lymph node dissection performed:<br>&nbsp;&nbsp;&nbsp;(no positive nodes found)","Lymph node dissection performed:<br>&nbsp;&nbsp;&nbsp;(positive nodes found)","No lymph node dissection performed")
)
# per meeting on 2014-12-19 with Jessica/Aline - if no node disection done, set any.positive.nodes to unknown
emdb$any.positive.nodes[emdb$any.positive.nodes=="No dissection done"] <- MISSING.EXPLICIT
emdb$any.positive.nodes <- factor(emdb$any.positive.nodes,levels=names(table(emdb$any.positive.nodes)))
emdb$any.positive.nodes_bXvS <- sapply(emdb$any.positive.nodes %in% ALL.MISSING.CODES,function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))}) 

emdb$any.positive.nodes_NArm<-factor(gsub("N/A",NA,emdb$any.positive.nodes))

# MMR - 4 proteins
MMR.IHC.4 <- apply(emdb[,c("MLH1.ihc.consolidated.numeric","MSH2.ihc.consolidated.numeric","MSH6.ihc.consolidated.numeric","PMS2.ihc.consolidated.numeric")],1,function(x){
	if (sum(x %in% ALL.MISSING.CODES)>0) {
		return(MISSING.UNK)
	} else {
		return(ifelse(sum(as.numeric(x))<length(x),VALUE.CODING.MMR.MSI.HIGH,VALUE.CODING.MMR.MSS))
	}
})
# MMR - 2 proteins
MMR.IHC.2 <- apply(emdb[,c("MSH6.ihc.consolidated.numeric","PMS2.ihc.consolidated.numeric")],1,function(x){
	if (sum(x %in% ALL.MISSING.CODES)>0) {
		return(MISSING.UNK)
	} else {
		return(ifelse(sum(as.numeric(x))<length(x),VALUE.CODING.MMR.MSI.HIGH,VALUE.CODING.MMR.MSS))
	}
})
emdb <- cbind(emdb,MMR.IHC.4,MMR.IHC.2)
# please note: MMR.Status is a combination of MSI status + MMR IHC 

# STMN
emdb$stmn.ihc.consolidated.numeric[emdb$stmn.ihc.consolidated.numeric %in% ALL.MISSING.CODES] <- NA
emdb$stmn.ihc.value.label[         emdb$stmn.ihc.value.label          %in% ALL.MISSING.CODES] <- NA
emdb$stmn.ihc.value.label <- factor(emdb$stmn.ihc.value.label, levels=c("Negative", "Weak staining", "Moderate staining", "Strong staining"))
emdb$stmn_b0v123 <- as.numeric(emdb$stmn.ihc.consolidated.numeric>0)

# L1CAM
emdb$l1cam.ihc.consolidated.numeric[emdb$l1cam.ihc.consolidated.numeric %in% ALL.MISSING.CODES] <- NA
emdb$l1cam.ihc.value.label[         emdb$l1cam.ihc.value.label          %in% ALL.MISSING.CODES] <- NA
emdb$l1cam.ihc.value.label.for.km <- factor(emdb$l1cam.ihc.value.label, levels=c("0%", "<10%", "10-50%", ">50%"))
emdb$l1cam.ihc.value.label[emdb$l1cam.ihc.value.label==">50%"] <- "&#62;50%"
emdb$l1cam.ihc.value.label <- factor(emdb$l1cam.ihc.value.label, levels=c("0%", "<10%", "10-50%", "&#62;50%"))
emdb$l1cam_b0v123 <- as.numeric(emdb$l1cam.ihc.consolidated.numeric>2)

# PR
emdb$PR.ihc.consolidated.numeric[emdb$PR.ihc.consolidated.numeric %in% ALL.MISSING.CODES] <- NA
emdb$PR.ihc.value.label[emdb$PR.ihc.value.label %in% ALL.MISSING.CODES] <- NA
emdb$PR.ihc.value.label <- factor(emdb$PR.ihc.value.label,levels=c("Negative","Positive"))

###############################
### helper functions        ###
#
# calculate survival time summaries
AssessSurvTime<-function(T1,T2,status){ 
	require(survival)
	# in case there are any is.na(status)
	# T2 may be NA as well for rfs!!!
	non.missing.cases <- !is.na(status) & !is.na(T2)
	T1 <- T1[non.missing.cases]
	T2 <- T2[non.missing.cases]
	status <- status[non.missing.cases]
	
	Otime=T2-T1 # observation time
	Stime=T2[status]-T1[status] # censoring time
	Etime=max(T2)-T1 # time to end of study
	SurvTime=T2-T1
	KFT=SurvTime; KFT[status] = T2[status]-T1[status] # known function time
	rev.status=rep(1,length(status));rev.status[status]=0
	Ftime=survfit(Surv(as.numeric(SurvTime),rev.status)~1)
	SumServ=read.table(textConnection(capture.output(Ftime)),skip=2,header=TRUE)
	
	MedianTime=list(
			Otime=as.numeric(round(median(Otime, na.rm=T)/365.24,2)),
			Stime=as.numeric(round(median(Stime, na.rm=T)/365.24,2)),
			Etime=as.numeric(round(median(Etime, na.rm=T)/365.24,2)),
			KFT=as.numeric(round(median(KFT, na.rm=T)/365.24,2)), 
			RevKM=as.numeric(round(SumServ[,"median"]/365.24,2)))
	return(MedianTime)
}

### end of helper functions ###
###############################

####################################
### define classifier models !!! ###
# define classifier models
source(CLASSIFIERMODELS.R)
#####################################


# if classify first, means did NOT exclude any cases with missing biomarkers ... now is the time to do it
	
cat("# starting with n=",nrow(emdbJPath),"(J Path), exclude",length(FindNeoAdj),"case with neo-adjuvant, and exclude cases that are classifiable for different models ...\n")
	
print.case.selection <- FALSE
if(!exists("RUN.IN.MARKDOWN")) {
	print.case.selection <- TRUE
} else {
	print.case.selection <- !RUN.IN.MARKDOWN
}
	
# per email from Jessica 2015-01-26 ... do not remove FISH missing ...
# just want to have interpretable POLE, MMR, p53 IHC
#for (i in 1:nrow(CLASS.NAMES)) {
	#class.name <- CLASS.NAMES[i,1]
	#if(print.case.selection) {
	#	cat("# removing",sum(is.na(emdb[,class.name])),"cases classifiable for model:",CLASS.NAMES[i,2],"\n")
	#}
	#emdb <- emdb[!is.na(emdb[,class.name]),]
#}
exclude.missing.pole.p53 <- which(emdb$POLE.mut.consolidated.numeric %in% ALL.MISSING.CODES | emdb$TP53.mut.consolidated.numeric%in% ALL.MISSING.CODES)
cat("# removing",length(exclude.missing.pole.p53),"case(s) with missing POLE/TP53 ...",paste(emdb$Study.Identifier[exclude.missing.pole.p53],collapse=", "),"\n")
emdb <- emdb[-exclude.missing.pole.p53,]
exclude.missing.mmr <- which(emdb$MMR.IHC.4 %in% ALL.MISSING.CODES)
cat("# removing",length(exclude.missing.mmr),"case(s) with missing MMR",paste(emdb$Study.Identifier[exclude.missing.mmr],collapse=", "),"\n")
emdb <- emdb[-exclude.missing.mmr,]
#exclude.missing.p53.ihc <- which(emdb$p53.ihc.consolidated.numeric %in% ALL.MISSING.CODES)
#cat("# removing",length(exclude.missing.p53.ihc),"case(s) with missing p53 IHC",paste(emdb$Study.Identifier[exclude.missing.p53.ihc],collapse=", "),"\n")
#emdb <- emdb[-exclude.missing.p53.ihc,]
	
# per meeting 2015-02-05 ... use the most cases for all classifiers and allow NA's to be in some classifiers
# e.g. allow cases with missing p53 IHC but available FISH data so that these cases would be unclassiable using
# model with p53 IHC but will be classifiable using model with FISH
	


### censoring  
#Year=as.numeric(substr(emdb$followup.start.date.mm.dd.yyyy,1,4))
Year   =as.numeric(format(as.Date(          emdb$followup.start.date.mm.dd.yyyy,format=DATE.FORMAT),"%Y")) # extract Year from followup.start.date
pl2   =Year   <2014-2; 
emdb=emdb[pl2,];
cutoffDate <- sapply(Year[pl2]+5,function(x){return(paste("12/31/",x,sep=""))}) # different date format!!!

S=biostatUtil::defineEventDate(emdb, cutoff=cutoffDate, cutoff.date.format=DATE.FORMAT, event="RFS")
O=biostatUtil::defineEventDate(emdb, cutoff=cutoffDate, cutoff.date.format=DATE.FORMAT, event="OS")
D=biostatUtil::defineEventDate(emdb, cutoff=cutoffDate, cutoff.date.format=DATE.FORMAT, event="DSS")

emdb$rfs.yrs=S$ev.years   
emdb$rfs.sts=S$ev.status
emdb$rfs.sts.no.unk.yrs=emdb$rfs.sts; emdb$rfs.sts.no.unk.yrs[is.na(emdb$rfs.yrs)] <- NA # set cases with unk rfs.yrs to be NA for status as well.

emdb$os.yrs=O$ev.years
emdb$os.sts=O$ev.status

emdb$dss.yrs=D$ev.years
emdb$dss.sts=D$ev.status
# end of censoring

