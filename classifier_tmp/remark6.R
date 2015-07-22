# Classifier paper - Remark #6
# 
# Author: samuelc
###############################################################################

# Median Followup  (for whole cohort, POLE mt POLE wt)
# Median PFS  "
# Median OS  "  "
# Median DSS
select.pole.wt  <- emdb$POLE.mut.consolidated.numeric==0
select.pole.mut <- emdb$POLE.mut.consolidated.numeric==1
sdates.os          =biostatUtil::defineEventDate(emdb,                  cutoff=cutoffDate,                 cutoff.date.format=DATE.FORMAT, event="OS")
sdates.os.pole.wt  =biostatUtil::defineEventDate(emdb[select.pole.wt, ],cutoff=cutoffDate[select.pole.wt ],cutoff.date.format=DATE.FORMAT, event="OS") # excludes germline
sdates.os.pole.mut =biostatUtil::defineEventDate(emdb[select.pole.mut,],cutoff=cutoffDate[select.pole.mut],cutoff.date.format=DATE.FORMAT, event="OS") # excludes germline 

sdates.dss          =biostatUtil::defineEventDate(emdb,                  cutoff=cutoffDate,                 cutoff.date.format=DATE.FORMAT, event="DSS")
sdates.dss.pole.wt  =biostatUtil::defineEventDate(emdb[select.pole.wt, ],cutoff=cutoffDate[select.pole.wt ],cutoff.date.format=DATE.FORMAT, event="DSS") # excludes germline
sdates.dss.pole.mut =biostatUtil::defineEventDate(emdb[select.pole.mut,],cutoff=cutoffDate[select.pole.mut],cutoff.date.format=DATE.FORMAT, event="DSS") # excludes germline 

sdates.rfs          =biostatUtil::defineEventDate(emdb,                  cutoff=cutoffDate,                 cutoff.date.format=DATE.FORMAT, event="RFS")
sdates.rfs.pole.wt  =biostatUtil::defineEventDate(emdb[select.pole.wt, ],cutoff=cutoffDate[select.pole.wt ],cutoff.date.format=DATE.FORMAT, event="RFS") # excludes germline
sdates.rfs.pole.mut =biostatUtil::defineEventDate(emdb[select.pole.mut,],cutoff=cutoffDate[select.pole.mut],cutoff.date.format=DATE.FORMAT, event="RFS") # excludes germline 

###
# function to generate a table (values only, no html formatting) of median followup time
# for different survival end points.
#
# event = OS, DSS or RFS
#
item6.fu.summary <- function(event) {
	event.name=switch(event,OS="os.event",DSS="dss.event",RFS="rfs.event")
	temp.d <- emdb
	sdates.whole <- biostatUtil::defineEventDate(
		temp.d,
		cutoff=cutoffDate,
		cutoff.date.format=DATE.FORMAT,
		event=event
	)
	item6.fu.summary.values <- c(
		AssessSurvTime(
			as.Date(temp.d[,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),
			as.Date(sdates.whole$ev.Date),
			sdates.whole$ev.status==event.name
		),
		sum(!is.na(sdates.whole$ev.status) & !is.na(sdates.whole$ev.Date)))
	for (class.group in names(table(temp.d[,CLASS.CHOSEN]))) {
		select.class.group <- temp.d[,CLASS.CHOSEN]==class.group
		sdates.class.group=biostatUtil::defineEventDate(
				temp.d[select.class.group,],
			cutoff=cutoffDate[select.class.group],
			cutoff.date.format=DATE.FORMAT,
			event=event
		)
		item6.fu.summary.values <- rbind(item6.fu.summary.values,
			c(
				AssessSurvTime(
					as.Date(temp.d[select.class.group,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),
					as.Date(sdates.class.group$ev.Date),
					sdates.class.group$ev.status==event.name
				),
				sum(!is.na(sdates.class.group$ev.status) & !is.na(sdates.class.group$ev.Date))
			)
		)
	}
	rownames(item6.fu.summary.values) <- c("Total",names(table(temp.d[,CLASS.CHOSEN])))
	return(item6.fu.summary.values)
}

# generate a table of followup summary ...
fu.summary.table.column.names <- c("observation<br>time","censoring<br>time","time to<br>end of study","known function<br>time","reverse<br>Kaplan Meier","n")
item6.fu.summary.table.os <- biostatUtil::tableAsHTML(
		item6.fu.summary("OS"), 
		caption="Overall survival",
		banded.row=TRUE, 
		column.names=fu.summary.table.column.names
)
item6.fu.summary.table.dss <- biostatUtil::tableAsHTML(
		item6.fu.summary("DSS"), 
		caption=paste("Disease specific survival (excluding ",sum(is.na(emdb$dss.sts))," cases with unknown deaths)",sep=""),
		banded.row=TRUE, 
		column.names=fu.summary.table.column.names
)
item6.fu.summary.table.rfs <- biostatUtil::tableAsHTML(
		item6.fu.summary("RFS"), 
		caption=paste("Relapse free survival (excluding ",sum(is.na(emdb$rfs.sts) | is.na(emdb$rfs.yrs))," cases with missing relapse time and/or events)",sep=""),
		banded.row=TRUE, 
		column.names=fu.summary.table.column.names
)
