# POLE paper - Remark #9 sample size
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

# Overall survival
item9.events.table <- do.cohort.characteristics(emdb, "POLE.mut.germline.as.missing", "POLE", 
		c("os.sts","dss.sts","rfs.sts.no.unk.yrs"), 
		c(FALSE,FALSE,FALSE), 
		c("Overall survival","Disease specific survival","Relapse free survival"),
		caption="Survival events among POLE wild type / mutated cases",
		missing.codes=c(),
		show.missing=TRUE,
		banded.rows=TRUE
)

##########################################
# power calculation of survival analysis
# 
calculate.power <- function(
	hrs, # hazard ratios  
	alpha, # alpha value e.g. 0.05
	surv.type="os" # end point: os, dss, rfs
) {
	switch(surv.type,
		os ={temp.d <- emdb                                              },
		dss={temp.d <- emdb[!is.na(emdb$dss.sts),                       ]},
		rfs={temp.d <- emdb[!is.na(emdb$rfs.sts) & !is.na(emdb$rfs.yrs),]}
	)
	nE=sum(temp.d$POLE.mut.germline.as.missing=="mutated",  na.rm=T)
	nC=sum(temp.d$POLE.mut.germline.as.missing=="wild type",na.rm=T)
	switch(surv.type,
		os={
			pE=sum(temp.d$os.sts[temp.d$POLE.mut.germline.as.missing=="mutated"  ]=="os.event")/nE
			pC=sum(temp.d$os.sts[temp.d$POLE.mut.germline.as.missing=="wild type"]=="os.event")/nC			
		},
		dss={
			pE=sum(temp.d$dss.sts[temp.d$POLE.mut.germline.as.missing=="mutated"  ]=="dss.event")/nE
			pC=sum(temp.d$dss.sts[temp.d$POLE.mut.germline.as.missing=="wild type"]=="dss.event")/nC			
		},
		rfs={
			pE=sum(temp.d$rfs.sts[temp.d$POLE.mut.germline.as.missing=="mutated"  ]=="rfs.event")/nE
			pC=sum(temp.d$rfs.sts[temp.d$POLE.mut.germline.as.missing=="wild type"]=="rfs.event")/nC			
		}
	)
	return(sapply(hrs,function(x){return(powerCT.default(nE,nC, pE, pC, x, alpha=alpha))}))
}

###################################################
# plot hr vs. power given a alpha
#
plot.calculate.power <- function(
	hrs, # hazard ratios  
	alpha, # alpha value e.g. 0.05
	surv.type="os", # end point: os, dss, rfs
	title="",
	flip.x=FALSE, # whether to PLOT the inverse of the x axis
	...
) {
	title <- trim.white.spaces(title)
	y <- calculate.power(hrs,alpha,surv.type)
	x <- hrs
	if (flip.x) {
		x <- 1/hrs
	}
	plot(
		x,
		y,
		type="n",
		main=paste(title,ifelse(nchar(title)>0," (",""),toupper(surv.type),", sig. level=",alpha,ifelse(nchar(title)>0,")",""),sep=""),
		xlab="Hazard ratio",
		ylab="Power",...)
	lines(x, y, type="l",...) 		
}



