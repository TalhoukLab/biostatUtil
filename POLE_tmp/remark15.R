# POLE paper - Remark #15 (univariable survival analysis)
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

# Age
# BMI
# Stage (Will group togethers for all stage I, stage II etc-not substages)
# Grade
# Histol
# LVSI
# any positive nodes
# NO treatment
# RT only
# Chemo only
# Both
# Unknown treatment
# MMR IHC loss
# MMR IHC intact  (still deciding if we will share this in this paper)
uni.cox.summary.all.variables <- doCoxphGeneric( # a table of univariable cox models of POLE and other variables
		emdb, 
		c("POLE.mut.germline.as.missing","age.at.surgery","bmi","stage_b1v234","Tumour.Grade_b12v3","Histological.Subtype_non_endo","LVSI","any.positive.nodes","any.init.treatment"), 
		c(
			HTML.COX.VAR.DESC.POLE,
			HTML.COX.VAR.DESC.AGE,
			"BMI<br>continuous",
			HTML.COX.VAR.DESC.STAGE,
			HTML.COX.VAR.DESC.GRADE,
			HTML.COX.VAR.DESC.HIST,
			HTML.COX.VAR.DESC.LVSI,
			HTML.COX.VAR.DESC.ANY.POS.NODES,
			HTML.COX.VAR.DESC.ANY.INIT.ADJ.TX
		),
		use.firth=FIRTH.THRESHOLD,
		stat.test="logtest",
		var.ref.groups=c(POLE.REF.GROUP,NA,NA,NA,NA,NA,NA,NA,NA),
		caption="Univariable analyses of relation of POLE mutation status and standard variables to OS/DSS/RFS in whole cohort",
		banded.rows=TRUE)

uni.cox.summary.all.variables.no.init.treatment <- doCoxphGeneric( # a table of univariable cox models of POLE and other variables
		emdb[emdb$any.init.treatment=="no.treatment",], 
		c("POLE.mut.germline.as.missing"                 ),#,"age.at.surgery","bmi","stage","Tumour.Grade","LVSI","MMR.IHC"), 
		c("POLE mutation status<br>wild type=0/mutated=1"),#,"age at surgery","BMI","stage","grade",       "LVSI","MMR<br>intact=0/absent=1"),
		use.firth=FIRTH.THRESHOLD,
		stat.test="logtest",
		var.ref.groups=c(POLE.REF.GROUP),
		caption="Univariable analyses of relation of POLE mutation status and standard variables to OS/DSS/RFS in cases with no initial adjuvant treatment",
		banded.rows=FALSE)

uni.cox.summary.all.variables.any.init.treatment <- doCoxphGeneric( # a table of univariable cox models of POLE and other variables
		emdb[emdb$any.init.treatment=="any.treatment",], 
		c("POLE.mut.germline.as.missing"                 ),#"age.at.surgery","bmi","stage","Tumour.Grade","LVSI","MMR.IHC"), 
		c("POLE mutation status<br>wild type=0/mutated=1"),#"age at surgery","BMI","stage","grade",       "LVSI","MMR<br>intact=0/absent=1"),
		use.firth=FIRTH.THRESHOLD,
		stat.test="logtest",
		var.ref.groups=c(POLE.REF.GROUP),
		caption="Univariable analyses of relation of POLE mutation status and standard variables to OS/DSS/RFS in cases with any initial adjuvant treatment",
		banded.rows=FALSE)

# meeting 2015-01-09 ...
# Jessica/Aline wanted to know the patients in POLE mutated / no treatment 
# who were censored before 4 years
if (!RUN.IN.MARKDOWN) {
	temp.d <- emdb[!is.na(emdb$any.init.treatment=="no.treatment") & emdb$any.init.treatment=="no.treatment" & emdb$POLE.mut.germline.as.missing==VALUE.CODING.MUTATED & emdb$os.sts=="os.censor" & emdb$os.yrs<4,]
	cat("per meeting 2015-01-09, want to see cases with POLE mutated, no treatment, os censored before 4 years\n")
	cat(paste(temp.d$Study.Identifier,collapse=","))
	cat("\n")
	
}
# list of cases who are censored before 2 years
censored.bf.2.years.os  <- emdb[emdb$os.yrs<2  & emdb$os.sts == "os.censor",c("TMA","Study.Identifier","Date.of.Surgery.mm.dd.yyyy","POLE.mut.germline.as.missing")]
censored.bf.2.years.dss <- emdb[emdb$dss.yrs<2 & emdb$dss.sts=="dss.censor",c("TMA","Study.Identifier","Date.of.Surgery.mm.dd.yyyy","POLE.mut.germline.as.missing")]
censored.bf.2.years.rfs <- emdb[emdb$rfs.yrs<2 & emdb$rfs.sts=="rfs.censor",c("TMA","Study.Identifier","Date.of.Surgery.mm.dd.yyyy","POLE.mut.germline.as.missing")]

censored.bf.2.years.dss <- censored.bf.2.years.dss[!is.na(censored.bf.2.years.dss$Study.Identifier),]
censored.bf.2.years.rfs <- censored.bf.2.years.rfs[!is.na(censored.bf.2.years.rfs$Study.Identifier),]

censored.bf.2.years.os[ ,"POLE.mut.germline.as.missing"] <- sapply(censored.bf.2.years.os[ ,"POLE.mut.germline.as.missing"] ,as.character)
censored.bf.2.years.dss[,"POLE.mut.germline.as.missing"] <- sapply(censored.bf.2.years.dss[,"POLE.mut.germline.as.missing"] ,as.character)
censored.bf.2.years.rfs[,"POLE.mut.germline.as.missing"] <- sapply(censored.bf.2.years.rfs[,"POLE.mut.germline.as.missing"] ,as.character)

names(censored.bf.2.years.os ) <- c("TMA","Study ID","Date of Surgery","POLE")
names(censored.bf.2.years.dss) <- c("TMA","Study ID","Date of Surgery","POLE")
names(censored.bf.2.years.rfs) <- c("TMA","Study ID","Date of Surgery","POLE")

rownames(censored.bf.2.years.os ) <- NULL
rownames(censored.bf.2.years.dss) <- NULL
rownames(censored.bf.2.years.rfs) <- NULL
