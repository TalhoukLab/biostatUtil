# POLE paper - Remark #9 sample size
# 
# Author: samuelc
###############################################################################

# event count with different survival endpoints
item9.events.table <- biostatUtil::doCohortCharacteristics(emdb, CLASS.CHOSEN, CLASS.CHOSEN.NAME, 
	c("os.sts","dss.sts","rfs.sts.no.unk.yrs"), 
	c(FALSE,FALSE,FALSE), 
	c("Overall survival","Disease specific survival","Relapse free survival"),
	caption=paste("Survival events among the ",CLASS.NAME,sep=""),
	missing.codes=c(),
	show.missing=TRUE,
	banded.rows=TRUE
)

