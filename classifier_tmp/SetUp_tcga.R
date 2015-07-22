# setup TCGA related
# 
# Author: samuelc
###############################################################################

library(cgdsr) # library to access TCGA data

### some constants ###
DO.CENSORING.ON.TCGA <- FALSE # whether to do censoring (i.e. exclude cases with < 2 yrs potential followup and cut off at 5 yrs)
### end of some constants ###

# get data
mycgds = CGDS("http://www.cbioportal.org/public-portal/")

test(mycgds) # qc data
		
# Get list of cancer studies at server
CS=getCancerStudies(mycgds)
		
# Get available case lists (collection of samples) for a given cancer study
mycancerstudy = CS[grep("TCGA Uterine Corpus Endometrioid Carcinoma, containing 373 samples",CS[,"description"]),1]
CL=getCaseLists(mycgds,mycancerstudy)
mycaselist = CL[1,1]
		
# Get available genetic profiles
geneticProfiles = getGeneticProfiles(mycgds,mycancerstudy)
CNAprofile  = geneticProfiles[grep("Log2 copy-number values",            geneticProfiles[,"genetic_profile_name"]),"genetic_profile_id"]
Mutprofile  = geneticProfiles[grep("Mutations",                          geneticProfiles[,"genetic_profile_name"]),"genetic_profile_id"]
Protprofile = geneticProfiles[grep("protein/phosphoprotein level \\(RPPA\\)",geneticProfiles[,"genetic_profile_name"]),"genetic_profile_id"]

if (length(CNAprofile ) != 1) {cat("ERROR!!! failed to get CNAprofile!!!\n")}
if (length(Mutprofile ) != 1) {cat("ERROR!!! failed to get Mutprofile!!!\n")}
if (length(Protprofile) != 1) {cat("ERROR!!! failed to get Protprofile!!!\n")}

# Get data slices for a specified list of genes, genetic profile and case list
Mutdata=getProfileData(mycgds,c('TP53','POLE','PTEN'),Mutprofile,mycaselist);Mutdata=ifelse(is.na(Mutdata),0,1) # manually looked at Figure 2a in TCGA paper and among 232 cases, there is no missing POLE mutation
Mutdata=Mutdata[order(rownames(Mutdata)),]
colnames(Mutdata) <- paste(colnames(Mutdata),".mut.numeric",sep="")
CNAdata=getProfileData(mycgds,c('MYC',"CCNE1","ERBB2",'FGFR3','SOX17'),CNAprofile,mycaselist);CNAdata=CNAdata[order(rownames(CNAdata)),]
Protdata=getProfileData(mycgds,c('P53','PTEN'),Protprofile,mycaselist); Protdata=Protdata[order(rownames(Protdata)),];
		
# Get clinical data for the case list
clinicaldata = getClinicalData(mycgds,mycaselist);clinicaldata=clinicaldata[order(rownames(clinicaldata)),]
	
# merge all data!!!
if (all(rownames(clinicaldata)==rownames(Mutdata))& all(rownames(clinicaldata)==rownames(CNAdata)) &all (rownames(clinicaldata)==rownames(Protdata))){
	TCGAfull=data.frame(clinicaldata,Mutdata,CNAdata,Protdata)
} else {
	cat("ERROR!!! something went wrong when trying to merge different data (e.g. mutation, copy number) data!!!\n")
}
# also do some formatting
TCGAfull <- cbind(TCGAfull,
	"DFS_YEARS"=TCGAfull$DFS_MONTHS/12,
	"OS_YEARS" =TCGAfull$OS_MONTHS/12,
	"POLE.phenotype"=factor(ifelse(TCGAfull$SUBTYPE=="POLE (Ultra-mutated)","phenotype","Not phenotype"),levels=c("phenotype","Not phenotype")),
	"POLE.mut.value.label"=factor(ifelse(TCGAfull$POLE==0,VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED),levels=c(VALUE.CODING.MUTATED,VALUE.CODING.WILD.TYPE))
)
msi.levels <- names(table(TCGAfull$MSI_STATUS_5_MARKER_CALL))
TCGAfull$MSI_STATUS_5_MARKER_CALL <- factor(sapply(TCGAfull$MSI_STATUS_5_MARKER_CALL,function(x){return(ifelse(x=="Indeterminant",NA,x))},USE.NAMES=FALSE),levels=msi.levels[msi.levels!="Indeterminant"])
TCGAfull$MSI_STATUS_7_MARKER_CALL <- factor(sapply(TCGAfull$MSI_STATUS_7_MARKER_CALL,function(x){return(ifelse(x=="Indeterminant",NA,x))},USE.NAMES=FALSE),levels=msi.levels[msi.levels!="Indeterminant"])
# order the subtype
TCGAfull$SUBTYPE <- factor(TCGAfull$SUBTYPE,levels=c("POLE (Ultra-mutated)", "MSI (Hyper-mutated)", "Copy-number low (Endometriod)", "Copy-number high (Serous-like)"))
TCGAfull$SUBTYPE.SIMPLE.NAME <- factor(sapply(TCGAfull$SUBTYPE,function(x){return(switch(as.character(x),
	"POLE (Ultra-mutated)"=CLASS.NAME.POLE, 
	"MSI (Hyper-mutated)"=CLASS.NAME.MSI, 
	"Copy-number low (Endometriod)"=CLASS.NAME.CNLOW, 
	"Copy-number high (Serous-like)"=CLASS.NAME.CNHIGH 
))},USE.NAMES=FALSE),levels=CLASS.ORDER)
# change OS/DFS status to match emdb$os.sts and emdb$rfs.sts
TCGAfull$OS_STATUS[TCGAfull$OS_STATUS=="Deceased"] <- "os.event"
TCGAfull$OS_STATUS[TCGAfull$OS_STATUS=="Living"]   <- "os.censor"
TCGAfull$DFS_STATUS[TCGAfull$DFS_STATUS=="Recurred"]    <- "rfs.event"
TCGAfull$DFS_STATUS[TCGAfull$DFS_STATUS=="DiseaseFree"] <- "rfs.censor"
TCGAfull$DFS_STATUS[TCGAfull$DFS_STATUS==""]            <- NA



vars=c("CNA_CLUSTER_K4","MSI_STATUS_5_MARKER_CALL","OS_MONTHS","OS_STATUS","DFS_STATUS","DFS_MONTHS","TP53","POLE","PTEN","SUBTYPE")

### TCGA related constants ###
TCGA.CLASS.NAME.GOLD <- "TCGA molecular classifier"

MSI.VAR.NAME <- "MSI_STATUS_5_MARKER_CALL" # define the MSI variable to use ... MSI_STATUS_5_MARKER_CALL or MSI_STATUS_7_MARKER_CALL
# use MSI_STATUS_5_MARKER_CALL confirmed by Aline email 2015-02-06
CN.GENES <- c("FGFR3","MYC","SOX17") # manually checked and there are NO missing value for FGFR1/MYC/SOX17
### end of TCGA related constants ###

#### define TCGA models ###

# do copy number assignment by clustering three gene values
# manually checked and there are NO missing value for FGFR1/MYC/SOX17
#
# function to do gene cluster to get cn high/low group
# - assume NO MISSING
get.cn.cluster.group <- function(input.d, genes, type="block", ...) {
	if (type=="kmeans") {
		cluster.obj <- kmeans(input.d[,genes],2,iter.max = 1000, ...)
		mean.centers <- apply(cluster.obj$centers,1,mean)
		high.group <- which(max(mean.centers)==mean.centers)
		return(as.numeric(cluster.obj$cluster==high.group))
	} else if (type=="block") {
		# block cluster
		out<-cocluster(as.matrix(input.d[,genes]),datatype="continuous",nbcocluster=c(3,1), ...)
		# Aline email 2015-02-10 ... explains why use nbcocluster=c(3,1) since manually looked and if
		# split with 2 groups, many false positive.
		CNAclass=out["rowclass"] # CNAclass starts at 0
		maxclass=which.max(out["classmean"])-1 #maxclass=unique(CNAclass)[which.max(out["classmean"])]
		return(ifelse(CNAclass==maxclass,1,0))
	} else {
		return(NA)
	}
}
TCGAfull$CN <- get.cn.cluster.group(TCGAfull,CN.GENES)

TCGA.CLASS.NAME.MSI.POLE.P53      <- "MSI/POLE/p53"      # 1
TCGA.CLASS.NAME.MSI.POLE.CN       <- "MSI/POLE/CN"       # 2
TCGA.CLASS.NAME.MSI.POLE.PTEN.P53 <- "MSI/POLE+PTEN/p53" # 3
TCGA.CLASS.NAME.MSI.POLE.PTEN.CN  <- "MSI/POLE+PTEN/CN"  # 4
TCGA.CLASS.NAME.POLE.MSI.P53      <- "POLE/MSI/p53"      # 5
TCGA.CLASS.NAME.POLE.MSI.CN       <- "POLE/MSI/CN"       # 6
TCGA.CLASS.NAME.POLE.PTEN.MSI.P53 <- "POLE+PTEN/MSI/p53" # 7
TCGA.CLASS.NAME.POLE.PTEN.MSI.CN  <- "POLE+PTEN/MSI/CN"  # 8
TCGA.CLASS.NAMES <- c(
	"tcga.class.msi.pole.p53",     TCGA.CLASS.NAME.MSI.POLE.P53,
	"tcga.class.msi.pole.cn",      TCGA.CLASS.NAME.MSI.POLE.CN,
	"tcga.class.msi.pole.pten.p53",TCGA.CLASS.NAME.MSI.POLE.PTEN.P53,
	"tcga.class.msi.pole.pten.cn", TCGA.CLASS.NAME.MSI.POLE.PTEN.CN,
	"tcga.class.pole.msi.p53",     TCGA.CLASS.NAME.POLE.MSI.P53,
	"tcga.class.pole.msi.cn",      TCGA.CLASS.NAME.POLE.MSI.CN,
	"tcga.class.pole.pten.msi.p53",TCGA.CLASS.NAME.POLE.PTEN.MSI.P53,
	"tcga.class.pole.pten.msi.cn", TCGA.CLASS.NAME.POLE.PTEN.MSI.CN
)
dim(TCGA.CLASS.NAMES) <- c(2,length(TCGA.CLASS.NAMES)/2)
TCGA.CLASS.NAMES <- t(TCGA.CLASS.NAMES)
TCGA.CLASS.CHOSEN <- TCGA.CLASS.NAMES[TCGA.CLASS.NAMES[,2]==TCGA.CLASS.NAME.MSI.POLE.P53,1]
TCGA.CLASS.CHOSEN.NAME <- TCGA.CLASS.NAMES[TCGA.CLASS.NAMES[,1]==TCGA.CLASS.CHOSEN,2]

# make sure all missing value to NA because EndSub and EndSub expects input to be TRUE/FALSE/NA

generate.classifier.models <- function(input.d, class.name=NA, cn.var.name="CN") {
	if (is.na(class.name)) {
		return(cbind(input.d,
			## Scenario 1&5 : MSI/POLE/P53 & POLE/MSI/P53
			"tcga.class.msi.pole.p53"     =EndSub( PolePhen=input.d$POLE.mut.numeric==1,                             MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d$TP53.mut.numeric==1),
			"tcga.class.pole.msi.p53"     =EndSub2(PolePhen=input.d$POLE.mut.numeric==1,                             MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d$TP53.mut.numeric==1),
			## Scenario 2&6 : MSI/POLE/CN & POLE/MSI/CN
			"tcga.class.msi.pole.cn"      =EndSub( PolePhen=input.d$POLE.mut.numeric==1,                             MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d[,cn.var.name]==1   ),
			"tcga.class.pole.msi.cn"      =EndSub2(PolePhen=input.d$POLE.mut.numeric==1,                             MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d[,cn.var.name]==1   ),
			## Scenario 3&7: MSI/POLE+PTEN/p53 & POLE+PTEN/MSI/p53
			"tcga.class.msi.pole.pten.p53"=EndSub( PolePhen=input.d$POLE.mut.numeric==1&input.d$PTEN.mut.numeric==1, MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d$TP53.mut.numeric==1),
			"tcga.class.pole.pten.msi.p53"=EndSub2(PolePhen=input.d$POLE.mut.numeric==1&input.d$PTEN.mut.numeric==1, MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d$TP53.mut.numeric==1),	
			## Scenario 4&8 : MSI/POLE+PTEN/CN & POLE+PTEN/MSI/CN
			"tcga.class.msi.pole.pten.cn" =EndSub( PolePhen=input.d$POLE.mut.numeric==1&input.d$PTEN.mut.numeric==1, MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d[,cn.var.name]==1   ),
			"tcga.class.pole.pten.msi.cn" =EndSub2(PolePhen=input.d$POLE.mut.numeric==1&input.d$PTEN.mut.numeric==1, MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d[,cn.var.name]==1   )
		))
	} else if (class.name == "tcga.class.msi.pole.p53"){
		return(cbind(input.d,"tcga.class.msi.pole.p53"     =EndSub( PolePhen=input.d$POLE.mut.numeric==1,                             MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d$TP53.mut.numeric==1)))
	} else if (class.name == "tcga.class.pole.msi.p53") {
		return(cbind(input.d,"tcga.class.pole.msi.p53"     =EndSub2(PolePhen=input.d$POLE.mut.numeric==1,                             MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d$TP53.mut.numeric==1)))
	} else if (class.name == "tcga.class.msi.pole.cn" ) {
		return(cbind(input.d,"tcga.class.msi.pole.cn"      =EndSub( PolePhen=input.d$POLE.mut.numeric==1,                             MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d[,cn.var.name]==1   )))
	} else if (class.name== "tcga.class.pole.msi.cn") {
		return(cbind(input.d,"tcga.class.pole.msi.cn"      =EndSub2(PolePhen=input.d$POLE.mut.numeric==1,                             MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d[,cn.var.name]==1   )))
	} else if (class.name=="tcga.class.msi.pole.pten.p53") {
		return(cbind(input.d,"tcga.class.msi.pole.pten.p53"=EndSub( PolePhen=input.d$POLE.mut.numeric==1&input.d$PTEN.mut.numeric==1, MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d$TP53.mut.numeric==1)))
	} else if (class.name=="tcga.class.pole.pten.msi.p53") {
		return(cbind(input.d,"tcga.class.pole.pten.msi.p53"=EndSub2(PolePhen=input.d$POLE.mut.numeric==1&input.d$PTEN.mut.numeric==1, MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d$TP53.mut.numeric==1)))
	} else if (class.name=="tcga.class.msi.pole.pten.cn") {
		return(cbind(input.d,"tcga.class.msi.pole.pten.cn" =EndSub( PolePhen=input.d$POLE.mut.numeric==1&input.d$PTEN.mut.numeric==1, MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d[,cn.var.name]==1   )))
	} else if (class.name=="tcga.class.pole.pten.msi.cn") {
		return(cbind(input.d,"tcga.class.pole.pten.msi.cn" =EndSub2(PolePhen=input.d$POLE.mut.numeric==1&input.d$PTEN.mut.numeric==1, MSIPhen=input.d[,MSI.VAR.NAME]==VALUE.CODING.TCGA.MSI.HIGH,CNHPhen=input.d[,cn.var.name]==1   )))
	} else {
		return(input.d) # no need to classifer!!!
	}
}

##################################################################
# merged data (from package)with clinical subtypes received from Jessica 2014-12-13
TCGAfull <- tcgaEd

### censoring of TCGA data per email from Aline 2015-02-16 ###
# NOTE: cannot remove <2 yrs potential followup since there's not dates
#       available from TCGA dataset
# cut off at 5 yrs
if (DO.CENSORING.ON.TCGA) {
	TCGA.CUT.OFF.YRS <- 5
	# OS
	TCGAfull$OS_STATUS <- apply(TCGAfull[,c("OS_YEARS","OS_STATUS")],1,function(x){
		if (x[1]>5) {
			return("os.censor")
		} else {
			return(x[2])
		}			
	})
	TCGAfull$OS_YEARS <- sapply(TCGAfull$OS_YEARS,function(x){return(min(x,TCGA.CUT.OFF.YRS))})
	# RFS
	TCGAfull$DFS_STATUS <- apply(TCGAfull[,c("DFS_YEARS","DFS_STATUS")],1,function(x){
		if (is.na(x[1])) {
			return(NA)
		} else if (x[1]>5) {
			return("rfs.censor")
		} else {
			return(x[2])
		}			
	})
	TCGAfull$DFS_YEARS <- sapply(TCGAfull$DFS_YEARS,function(x){return(min(x,TCGA.CUT.OFF.YRS))})
}
### end of censoring of TCGA data per email from Aline 2015-02-16 ###

##################################################
# change survival endpoint time/status variables to match doKMPlots
names(TCGAfull)[names(TCGAfull)=="OS_YEARS"] <- "os.yrs"
names(TCGAfull)[names(TCGAfull)=="OS_STATUS"] <- "os.sts"
names(TCGAfull)[names(TCGAfull)=="DFS_YEARS"] <- "rfs.yrs"
names(TCGAfull)[names(TCGAfull)=="DFS_STATUS"] <- "rfs.sts"


