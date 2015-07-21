# POLE paper - Remark #8
# 
# Author: samuelc
###############################################################################
if (!exists("RUN.IN.MARKDOWN")) {
	# if variable does not exist, that means we are not running in the markdown file
	RUN.IN.MARKDOWN <- FALSE # indicate whether we are running the files in the marked down file
	# do set up if not fun in markdown
	# this is for code development & testing ... so assume this will never get called from 
	# Aline's computer
	source('SetUp_db.R')
}

# Median Followup  (for whole cohort, POLE mt POLE wt)
# Median PFS  "
# Median OS  "  "
# Median DSS
select.pole.wt  <- emdb$POLE.mut.consolidated.numeric==0
select.pole.mut <- emdb$POLE.mut.consolidated.numeric==1
sdates.os          =defineEventDate(emdb,                  cutoff=cutoffDate,                 cutoff.date.format=DATE.FORMAT, event="OS")
sdates.os.pole.wt  =defineEventDate(emdb[select.pole.wt, ],cutoff=cutoffDate[select.pole.wt ],cutoff.date.format=DATE.FORMAT, event="OS") # excludes germline
sdates.os.pole.mut =defineEventDate(emdb[select.pole.mut,],cutoff=cutoffDate[select.pole.mut],cutoff.date.format=DATE.FORMAT, event="OS") # excludes germline 

sdates.dss          =defineEventDate(emdb,                  cutoff=cutoffDate,                 cutoff.date.format=DATE.FORMAT, event="DSS")
sdates.dss.pole.wt  =defineEventDate(emdb[select.pole.wt, ],cutoff=cutoffDate[select.pole.wt ],cutoff.date.format=DATE.FORMAT, event="DSS") # excludes germline
sdates.dss.pole.mut =defineEventDate(emdb[select.pole.mut,],cutoff=cutoffDate[select.pole.mut],cutoff.date.format=DATE.FORMAT, event="DSS") # excludes germline 

sdates.rfs          =defineEventDate(emdb,                  cutoff=cutoffDate,                 cutoff.date.format=DATE.FORMAT, event="RFS")
sdates.rfs.pole.wt  =defineEventDate(emdb[select.pole.wt, ],cutoff=cutoffDate[select.pole.wt ],cutoff.date.format=DATE.FORMAT, event="RFS") # excludes germline
sdates.rfs.pole.mut =defineEventDate(emdb[select.pole.mut,],cutoff=cutoffDate[select.pole.mut],cutoff.date.format=DATE.FORMAT, event="RFS") # excludes germline 

# generate a table of followup summary ...
fu.summary.table.column.names <- c("observation<br>time","censoring<br>time","time to<br>end of study","known function<br>time","reverse<br>Kaplan Meier","n")
item6.fu.summary.table.os <- tableAsHTML(
	rbind(
		"whole"         =c(
			AssessSurvTime(as.Date(emdb[,               "followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.os$ev.Date         ),sdates.os$ev.status         =="os.event" ),
			sum(!is.na(sdates.os$ev.status))),
		"POLE wild type"=c(
			AssessSurvTime(as.Date(emdb[select.pole.wt, "followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.os.pole.wt$ev.Date ),sdates.os.pole.wt$ev.status =="os.event" ),
			sum(!is.na(sdates.os.pole.wt$ev.status))),
		"POLE mutation" =c(
			AssessSurvTime(as.Date(emdb[select.pole.mut,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.os.pole.mut$ev.Date),sdates.os.pole.mut$ev.status=="os.event" ),
			sum(!is.na(sdates.os.pole.mut$ev.status)))
	), 
	caption="Overall survival",
	banded.row=TRUE, 
	column.names=fu.summary.table.column.names
)
item6.fu.summary.table.dss <- tableAsHTML(
	rbind(
		"whole"         =c(
			AssessSurvTime(as.Date(emdb[,               "followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.dss$ev.Date         ),sdates.dss$ev.status         =="dss.event" ),
			sum(!is.na(sdates.dss$ev.status))),
		"POLE wild type"=c(
			AssessSurvTime(as.Date(emdb[select.pole.wt, "followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.dss.pole.wt$ev.Date ),sdates.dss.pole.wt$ev.status =="dss.event" ),
			sum(!is.na(sdates.dss.pole.wt$ev.status))),
		"POLE mutation" =c(
			AssessSurvTime(as.Date(emdb[select.pole.mut,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.dss.pole.mut$ev.Date),sdates.dss.pole.mut$ev.status=="dss.event" ),
			sum(!is.na(sdates.dss.pole.mut$ev.status)))
	), 
	caption=paste("Disease specific survival (excluding ",sum(is.na(emdb$dss.sts))," cases with unknown deaths)",sep=""),
	banded.row=TRUE, 
	column.names=fu.summary.table.column.names
)
item6.fu.summary.table.rfs <- tableAsHTML(
	rbind(
		"whole"         =c(
			AssessSurvTime(as.Date(emdb[,               "followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.rfs$ev.Date         ),sdates.rfs$ev.status         =="rfs.event" ),
			sum(!is.na(sdates.rfs$ev.status) & !is.na(sdates.rfs$ev.years))),
		"POLE wild type"=c(
			AssessSurvTime(as.Date(emdb[select.pole.wt, "followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.rfs.pole.wt$ev.Date ),sdates.rfs.pole.wt$ev.status =="rfs.event" ),
			sum(!is.na(sdates.rfs.pole.wt$ev.status) & !is.na(sdates.rfs.pole.wt$ev.years))),
		"POLE mutation" =c(
			AssessSurvTime(as.Date(emdb[select.pole.mut,"followup.start.date.mm.dd.yyyy"],format=DATE.FORMAT),as.Date(sdates.rfs.pole.mut$ev.Date),sdates.rfs.pole.mut$ev.status=="rfs.event" ),
			sum(!is.na(sdates.rfs.pole.mut$ev.status) & !is.na(sdates.rfs.pole.mut$ev.years)))
	), 
	caption=paste("Relapse free survival (excluding ",sum(is.na(emdb$rfs.sts) | is.na(emdb$rfs.yrs))," cases with missing relapse time and/or events)",sep=""),
	banded.row=TRUE, 
	column.names=fu.summary.table.column.names
)

