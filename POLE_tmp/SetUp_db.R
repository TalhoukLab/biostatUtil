# Database set e.g. do censoring, exclude cases ... etc
# 
# also get library files
#
# Author: samuelc
###############################################################################

# get library files
library(knitr)
if (!exists("LIB.DIR")) { # LIB.DIR is defined in markdown file
	LIB.DIR <- "R_scripts/" 
}
library(knitr)
library(powerSurvEpi)
library(logistf) # Firth correction for logistic regression
library(MASS) # for stepAIC()
curr.dir <- getwd()
setwd(curr.dir) # change directory back to current directory !!!

### constants ###
MISSING.EXPLICIT <- "N/A" # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING.UNK <- "Unk" # missing because values was not found (e.g. in data files) but the value must exist somewhere.
MISSING...NOT.FOUND.IN.DATA.FILE <- "" # data point not mentioned in data file.
MISSING.BIOMARKER.EXPLICIT <- MISSING.UNK # missing value code for values that are explicitily indicated as missing from data source e.g. "X" in grade
MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE <- "" # data point not mentioned in data file.
ALL.MISSING.CODES <- unique(c(MISSING.EXPLICIT, MISSING...NOT.FOUND.IN.DATA.FILE, MISSING.UNK, MISSING.BIOMARKER.EXPLICIT, MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE))

VALUE.CODING.WILD.TYPE <- "wild type"
VALUE.CODING.MUTATED <- "mutated"
VALUE.CODING.MMR.ABSENT <- "absent"
VALUE.CODING.MMR.INTACT <- "intact"
VALUE.CODING.MMR.MSI.HIGH <- "MSI-high"
VALUE.CODING.MMR.MSS <- "MSS"
VALUE.CODING.INIT.TREATMENT.NO <- "no.treatment"
VALUE.CODING.INIT.TREATMENT.CHEMO.ONLY <- "chemo.only"
VALUE.CODING.INIT.TREATMENT.RT.ONLY <- "rt.only"
VALUE.CODING.INIT.TREATMENT.VAG.BRACHY.ONLY <- "vag.brachy.only"
VALUE.CODING.INIT.TREATMENT.BOTH <- "both"
VALUE.CODING.INIT.TREATMENT.ANY <- "any.treatment"
VALUE.CODING.AVAILABLE <- "available"
VALUE.CODING.MISSING <- "missing"

POLE.REF.GROUP <- VALUE.CODING.WILD.TYPE

HTML.COX.VAR.DESC.POLE            <- "POLE mutation status<br>wild type (reference) vs. mutated"
HTML.COX.VAR.DESC.AGE             <- "Age at surgery<br>continuous"
HTML.COX.VAR.DESC.STAGE           <- "Stage<br>I (reference) vs. {II, III or IV}"
HTML.COX.VAR.DESC.GRADE           <- "Grade<br>{1 or 2} (reference) vs. 3"
HTML.COX.VAR.DESC.HIST            <- "Histological subtype<br>endometrioid (reference) vs. non-endometrioid"
HTML.COX.VAR.DESC.LVSI            <- "Lymphovascular invasion<br>(no lymphovascular invasion as reference)"
HTML.COX.VAR.DESC.ANY.POS.NODES   <- "Any positive nodes<br>(no positive nodes as reference)"
HTML.COX.VAR.DESC.ANY.INIT.ADJ.TX <- "Any initial adjuvant treatment<br>(no treatment as reference)"

TMA.NAME.09.004  <- "09-004 (Endometrial carcinoma)"
TMA.NAME.10.005  <- "10-005 (High grade endometrioid)"
TMA.NAME.10.006  <- "10-006 (High grade serous)"
TMA.NAME.10.008  <- "10-008 (Clear cell)"
TMA.NAME.11.010  <- "11-010 (VOA endometrial)"
TMA.NAME.14.001  <- "14-001 (VOA endometrial serous)"
ADDITIONAL.CASES <- "Additional cases not on TMA"

FIRTH.THRESHOLD <- 0.8 # percent of censor cases to use Firth correction in Cox model
FIRTH.CAPTION <- "<sup>(F)</sup>" # html text to be placed beside the p-value to indicate that the Cox model used Firth

NUM.DIGITS.TO.SHOW <- 1 # number of significant digits to show 

USE.ALINE.KM.PLOT <- TRUE

### end of constants ###

### READ DATA FILE ###
#ed=read.csv(ED.CSV, header=T, stringsAsFactors =FALSE) # read database file ...
library(bccaEndometrial)
data(ed)
ed.raw <- ed # absolutely no filtered out cases

# per meeting 2015-01-09, want to start with ALL POLE interpretable
ed=ed[!ed$POLE.mut.consolidated.numeric%in%c("1 germline", ALL.MISSING.CODES),]

### remove duplicates ###
FindDuplicates=grep("duplicate",ed[,"comment.about.duplicate"]) # Find Duplicates
FindCompMissing=which(is.na(as.Date(ed[,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT))) #Find Completely Missing
FindNeoAdj= which(as.Date(ed[,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT)<as.Date(ed[,"Date.of.Surgery.mm.dd.yyyy"],format=DATE.FORMAT))
emdb            =ed[-c(FindDuplicates,FindCompMissing,FindNeoAdj),] #filter out Duplicates, completely missing, Neoadjuvant)
emdb.w.duplicate=ed[-c(               FindCompMissing,FindNeoAdj),] 

# cohort without remove cases with short potential followup
emdb.no.censor <- emdb

### censoring  
#Year=as.numeric(substr(emdb$followup.start.date.mm.dd.yyyy,1,4))
Year   =as.numeric(format(as.Date(          emdb$followup.start.date.mm.dd.yyyy,format=DATE.FORMAT),"%Y")) # extract Year from followup.start.date
ed.Year=as.numeric(format(as.Date(emdb.w.duplicate$followup.start.date.mm.dd.yyyy,format=DATE.FORMAT),"%Y")) # extract Year from followup.start.date
pl2   =Year   <2014-2; 
ed.pl2=ed.Year<2014-2;
emdb=emdb[pl2,];
emdb.w.duplicate=emdb.w.duplicate[ed.pl2,]
cutoffDate <- sapply(Year[pl2]+5,function(x){paste("12/31/",x,sep="")}) # different date format!!!

# per email from Melissa 2015-01-13 ... export dataset
# WATCH OUT ... need to write emdb.w.duplicate (exclude duplicate POLE) since want to show 
#               cases in 11-010 (instead of 10-005/6) for duplicate cases.
if (!RUN.IN.MARKDOWN) {
	write.table(
		emdb.w.duplicate[
			!emdb.w.duplicate$POLE.mut.2014.11.08.numeric %in% ALL.MISSING.CODES & 
			emdb.w.duplicate$comment.about.duplicate!="first occurrence",
		],
		file=paste("R:\\data\\analyses\\endometrial\\paper_pole\\pole_paper_data_n",nrow(emdb),"_",strsplit(strsplit(ED.CSV,"_")[[1]][4],".csv")[[1]],".txt",sep=""),
		quote=TRUE,
		row.names=FALSE,
		col.names=TRUE,
		sep='\t')
}

S=defineEventDate(emdb, cutoff=cutoffDate, cutoff.date.format=DATE.FORMAT, event="RFS")
O=defineEventDate(emdb, cutoff=cutoffDate, cutoff.date.format=DATE.FORMAT, event="OS")
D=defineEventDate(emdb, cutoff=cutoffDate, cutoff.date.format=DATE.FORMAT, event="DSS")

emdb$rfs.yrs=S$ev.years   
emdb$rfs.sts=S$ev.status
emdb$rfs.sts.no.unk.yrs=emdb$rfs.sts; emdb$rfs.sts.no.unk.yrs[is.na(emdb$rfs.yrs)] <- NA # set cases with unk rfs.yrs to be NA for status as well.

emdb$os.yrs=O$ev.years
emdb$os.sts=O$ev.status

emdb$dss.yrs=D$ev.years
emdb$dss.sts=D$ev.status
# end of censoring

### factor ...
#   change some text values to factor so that when we do tables or km plots, 
#   they would appear in a preferred order 
# 
# POLE - lump missing with germline mutation
POLE.GERMLINE.FLAG <- "1 germline"
consolidate.pole.mv.germline.to.unk <- function(x){
	return (ifelse(
		x %in% c(ALL.MISSING.CODES,POLE.GERMLINE.FLAG), 
		MISSING.BIOMARKER.EXPLICIT, 
		ifelse(x==0,VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))
	)
}
emdb$POLE.mut.germline.as.missing             <- factor(sapply(            emdb$POLE.mut.consolidated.numeric,consolidate.pole.mv.germline.to.unk),levels=c(VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))
emdb.w.duplicate$POLE.mut.germline.as.missing <- factor(sapply(emdb.w.duplicate$POLE.mut.2014.11.08.numeric,  consolidate.pole.mv.germline.to.unk),levels=c(VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))

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



### other formatting & calculations ...
#
# make sure POLE.mut.consolidated.numeric is numeric
emdb$POLE.mut.consolidated.numeric <- as.numeric(emdb$POLE.mut.consolidated.numeric)
#
# year of diagnosis
# extract year from date in format of mm/dd/yyyy
emdb$dx.year   <- as.numeric(format(as.Date(emdb$followup.start.date.mm.dd.yyyy,format=DATE.FORMAT),"%Y"))
emdb$dx.decade <- cut(emdb$dx.year,c(min(emdb$dx.year)-1,1993,2003,2013),label=c("1983-1993","1994-2003","2004-2013"))

# age - change to numeric
emdb$age.at.surgery <- sapply(emdb$age.at.surgery, function(x){return(ifelse(x==MISSING.EXPLICIT,NA,as.numeric(x)))})
emdb$age.at.surgery_bXvS <- sapply(is.na(emdb$age.at.surgery),function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))})

# bmi - weight(kg)/(height(m))^2
emdb$bmi <- 
	sapply(emdb$Weight,function(x){return(ifelse(x %in% ALL.MISSING.CODES,NA,as.numeric(x)))})/
	sapply(emdb$Height,function(x){return(ifelse(x %in% ALL.MISSING.CODES,NA,(as.numeric(x)/100)^2))})
emdb$bmi_bXvS <- sapply(is.na(emdb$bmi),function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))})

# stage - group togethers for all stage I, stage II etc-not substages
#       - per meeting 2015-01-09, need to show distinction between stage IA and IB
emdb$stage <-
	factor(sapply(emdb$FIGO.Stage.2009,function(x){
		if (x %in% ALL.MISSING.CODES) {
			return(x) # preserve missing codes
		} else if (startsWith(x,"IV")){
			return("IV")
		} else if (startsWith(x,"III")) {
			return("III")
		} else if (startsWith(x,"II")) {
			return("II")
		} else if (startsWith(x,"IA")) {
			return("IA")
		} else if (startsWith(x,"IB")) {
			return("IB")
		} else {
			return(NA) # should not happen!!!
		}
	}),levels=c(MISSING.EXPLICIT, "IA","IB", "II", "III", "IV"))
emdb$stage_b1v234 <- factor(sapply(emdb$stage,function(x){
		x <- as.character(x)
		if (x %in% ALL.MISSING.CODES) {
			return(x) # preserve missing codes
		} else {
			return(ifelse(x%in%c("IA","IB"),"I","II, III or IV"))
		}
	}),levels=c(MISSING.EXPLICIT, "I","II, III or IV"))
emdb$stage_bXvS <- sapply(emdb$stage %in% ALL.MISSING.CODES,function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))})

# stage x age (for remark 14)
emdb$stage_x_age <- apply(emdb[,c("stage_b1v234","age.at.surgery")],1,function(x){
	if (x[1]%in%ALL.MISSING.CODES | is.na(x[2])) {
		return(NA)
	} else {
		if (x[1]=="I") {
			if (as.numeric(x[2])<=60) {
				return("Stage I / <=60 yrs")
			} else {
				return("Stage I / >60 yrs")
			}
		} else {
			if (as.numeric(x[2])<=60) {
				return("Stage II, III or IV / <=60 yrs")
			} else {
				return("Stage II, III or IV / >60 yrs")
			}
		}
	}			
})

# stage x bmi (for remark 14)
emdb$stage_x_bmi <- apply(emdb[,c("stage_b1v234","bmi")],1,function(x){
	if (x[1]%in%ALL.MISSING.CODES | is.na(x[2])) {
		return(NA)
	} else {
		if (x[1]=="I") {
			if (as.numeric(x[2])<30) {
				return("Stage I / <30 bmi")
			} else {
				return("Stage I / >=30 bmi")
			}
		} else {
			if (as.numeric(x[2])<30) {
				return("Stage II, III or IV / <30 bmi")
			} else {
				return("Stage II, III or IV / >=30 bmi")
			}
		}
	}					
})

# stage x age x bmi
emdb$stage_x_age_x_bmi <- apply(emdb[,c("stage_b1v234","age.at.surgery","bmi")],1,function(x){
	if (x[1]%in%ALL.MISSING.CODES | is.na(x[2]) | is.na(x[3])) {
		return(NA)		
	} else {
		if (x[1]=="I") {
			if (as.numeric(x[2])<=60) {
				if (as.numeric(x[3])<30) {
					return("Stage I / <=60 yrs / <30 bmi")
				} else {
					return("Stage I / <=60 yrs / >=30 bmi")
				}
			} else {
				if (as.numeric(x[3])<30) {
					return("Stage I / >60 yrs / <30 bmi")
				} else {
					return("Stage I / >60 yrs / >=30 bmi")
				}
			}
		} else {
			if (as.numeric(x[2])<=60) {
				if (as.numeric(x[3])<30) {
					return("Stage II, III or IV / <=60 yrs / <30 bmi")
				} else {
					return("Stage II, III or IV / <=60 yrs / >=30 bmi")
				}
			} else {
				if (as.numeric(x[3])<30) {
					return("Stage II, III or IV / >60 yrs / <30 bmi")
				} else {
					return("Stage II, III or IV / >60 yrs / >=30 bmi")
				}
			}
		}
	}
})

# age x bmi (for remark 14)
emdb$age_x_bmi <- apply(emdb[,c("age.at.surgery","bmi")],1,function(x){
	if (is.na(x[1]) | is.na(x[2])) {
		return(NA)
	} else {
		if (as.numeric(x[1])<=60) {
			if (as.numeric(x[2])<30) {
				return("age <=60 yrs / <30 bmi")
			} else {
				return("age <=60 yrs / >=30 bmi")
			}
		} else {
			if (as.numeric(x[2])<30) {
				return("age >60 yrs / <30 bmi")
			} else {
				return("age >60 yrs / >=30 bmi")
			}
		}
	}					
})

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
	"Clear Cell",
	#"Small Cell", # cases only appears among POLE missing
	"Mixed Endometrioid And Clear Cell",
	"Mixed Endometrioid With Undifferentiated",
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

# Grade {1/2 vs. 3} x Histology {endo vs. non-endo} per meeting 2015-01-09
emdb$grade_x_endo <- paste(emdb$Tumour.Grade_b12v3, "/", emdb$Histological.Subtype_non_endo)
emdb$grade_x_endo <- factor(emdb$grade_x_endo,levels=names(table(emdb$grade_x_endo)))

# Stage x Histology {endo vs. non-endo} per meeting 2015-05-27 (for remark 18, sensitivity analysis)
emdb$stage_x_endo <- paste("Stage", emdb$stage_b1v234, "/", emdb$Histological.Subtype_non_endo)
emdb$stage_x_endo[emdb$stage_x_endo=="Stage N/A / Endometrioid"] <- MISSING.EXPLICIT
emdb$stage_x_endo <- factor(emdb$stage_x_endo,levels=names(table(emdb$stage_x_endo)))

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

# MMR (MSI assay + IHC)
emdb$MMR.Status <- factor(emdb$MMR.Status,levels=c(MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE,MISSING.BIOMARKER.EXPLICIT,VALUE.CODING.MMR.MSI.HIGH,VALUE.CODING.MMR.MSS)) # keep missing coding!!!

# MMR IHC loss
# i.e. any MLH1, MSH2, MSH6, PMS2
emdb$MMR.IHC <- factor(emdb$MMR.IHC,levels=c(MISSING.BIOMARKER...NOT.FOUND.IN.DATA.FILE,MISSING.BIOMARKER.EXPLICIT,VALUE.CODING.MMR.INTACT,VALUE.CODING.MMR.ABSENT))
emdb$MMR.IHC_bXvS <- sapply(emdb$MMR.IHC %in% ALL.MISSING.CODES,function(x){return(ifelse(x,VALUE.CODING.MISSING,VALUE.CODING.AVAILABLE))}) 

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

