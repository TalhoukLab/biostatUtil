# function to calculate event/status given censor date
# original version received from Aline 2014-09-26
# 
# Author: samuelc
###############################################################################


#
# ed = input data
# event = event type: OS, DSS
# cutoff.date.format = date format of cut off date
# format.dates.in.ed = (TRUE/FALSE) indicate whether to format dates in ed
# cutoff.date.format = format of cutoff
# status.only = (TRUE/FALSE) indicating whether to return survival status only
#
# return
#    ev.status = string e.g. "os.censor", "os.event"
#    ev.Date = date object 
#
# example usage:
# 
# EXAMPLE 1. to get updated survival dates only for censor date of 2013-12-12 ...
# > as.Date(DefineEventDate(d,"2013-12-12",format.dates.in.ed=TRUE, status.only=FALSE)$ev.Date, origin="1970-01-01")
#
# Please note, because there may be some string i.e. "Unk" in the return date (ev.Date), R changed
#    the date to string ... therefore, you need to do a as.numeric() and a as.Date() to change
#    the ev.Date from string back to Date object. 
#
DefineEventDate <- function(
  ed,
  cutoff,
  event="OS",
  format.dates.in.ed=TRUE,
  cutoff.date.format="%Y-%m-%d",
  status.only=FALSE
){
  #  three possible events OS DSS and RFS
  ev.status=NULL
  ev.Date=NULL
  MISSING.UNK=NA #"Unk"
  
  # some constants ...
  OS.EVENT  <- "os.event"
  OS.CENSOR <- "os.censor"
  DSS.EVENT  <- "dss.event"
  DSS.CENSOR <- "dss.censor"
  RFS.EVENT  <- "rfs.event"
  RFS.CENSOR <- "rfs.censor"
  
  MISSING.UNK=NA #"Unk"
  ALL.MISSING.CODES=c("","N/A","Unk") # WARNING: make sur this is ALL the possible missing codes!!!
  
  # if cutoff is a single value, make repeats of it, otherwise just leave it.
  cutoff.length=length(cutoff)
  if (cutoff.length==1) {
    cutoff=rep(cutoff,nrow(ed))
  } else if (cutoff.length!=nrow(ed)) {
    print("ERROR!!! cutoff passed to DefineEventDate is neither a single value or a vector of length=# or rows of the passed in data matrix.  Please double check")
    return(NA)
  }
  
  # format date ...
  if (format.dates.in.ed) {
    cutoff <- as.Date(cutoff,format=cutoff.date.format)
    followup.start.date.mm.dd.yyyy <- as.Date(ed$followup.start.date.mm.dd.yyyy,format=cutoff.date.format) # missing value "" or "N/A" will return NA
    Date.of.Death.mm.dd.yyyy <- as.Date(ed$Date.of.Death.mm.dd.yyyy,format=cutoff.date.format) # missing value "" or "N/A" will return NA
    followup.max.date.mm.dd.yyyy <- as.Date(ed$followup.max.date.mm.dd.yyyy,format=cutoff.date.format)# missing value "" or "N/A" will return NA
    followup.end.date.mm.dd.yyyy <- as.Date(ed$followup.end.date.mm.dd.yyyy,format=cutoff.date.format)# missing value "" or "N/A" will return NA
    Date.of.First.Recorded.Recurrence.mm.dd.yyyy <- as.Date(ed$Date.of.First.Recorded.Recurrence.mm.dd.yyyy,format=cutoff.date.format)# missing value "" or "N/A" will return NA
  } else {
    followup.start.date.mm.dd.yyyy <- ed$followup.start.date.mm.dd.yyyy
    Date.of.Death.mm.dd.yyyy <- ed$Date.of.Death.mm.dd.yyyy
    followup.max.date.mm.dd.yyyy <- ed$followup.max.date.mm.dd.yyyy
    followup.end.date.mm.dd.yyyy <- ed$followup.end.date.mm.dd.yyyy
    Date.of.First.Recorded.Recurrence.mm.dd.yyyy <- ed$Date.of.First.Recorded.Recurrence.mm.dd.yyyy
  }
  
  #if(any(followup.start.date.mm.dd.yyyy>cutoff, na.rm=TRUE)){print("Error with cutoff date")}
  
  # reminder: watch out for scenario like the following:
  #           - Death on 2010-10-10
  #           - Last contact on 2009-10-10
  #           - censor date 2010-02-10
  #           - the censor date/status should be 2010-02-10/alive
  switch (event,
          OS={in.ev=
                ed$Status.at.Death %in% c("Dead of Disease","Dead of Intercurrent Disease", "Dead of Other", "Dead Unspecified") & 
                Date.of.Death.mm.dd.yyyy <= cutoff
              ev.status[in.ev]=OS.EVENT
              ev.Date[in.ev]=Date.of.Death.mm.dd.yyyy[in.ev] # return date objects
              
              in.cens1=
                ed$Status.at.Last.Contact %in% c("Alive No Evidence of Disease", "Alive Unspecified", "Alive With Disease", "Alive With Morbidity") & 
                followup.max.date.mm.dd.yyyy <= cutoff &
                !in.ev
              ev.status[in.cens1]=OS.CENSOR
              ev.Date[in.cens1]=followup.max.date.mm.dd.yyyy[in.cens1] # return date objects
              
              in.cens2=
                ed$Status.at.Death %in% c("Dead of Disease","Dead of Intercurrent Disease", "Dead of Other", "Dead Unspecified") & 
                Date.of.Death.mm.dd.yyyy > cutoff &
                !in.ev
              ev.status[in.cens2]=OS.CENSOR
              ev.Date[in.cens2]=cutoff[in.cens2]
              
              in.cens3=
                ed$Status.at.Last.Contact %in% c("Alive No Evidence of Disease", "Alive Unspecified", "Alive With Disease", "Alive With Morbidity") & 
                followup.max.date.mm.dd.yyyy > cutoff & 
                !in.ev
              ev.status[in.cens3]=OS.CENSOR
              ev.Date[in.cens3]=cutoff[in.cens3]
          },
          DSS={in.ev= ed$Status.at.Death %in% c("Dead of Disease") & 
                 Date.of.Death.mm.dd.yyyy <= cutoff
               ev.status[in.ev]=DSS.EVENT
               ev.Date[in.ev]=Date.of.Death.mm.dd.yyyy[in.ev] # return date objects
               
               in.cens1p0=
                 ed$Status.at.Last.Contact %in% c("Alive No Evidence of Disease", "Alive Unspecified", "Alive With Disease", "Alive With Morbidity") & 
                 followup.max.date.mm.dd.yyyy <= cutoff & 
                 !in.ev
               ev.status[in.cens1p0]=DSS.CENSOR
               ev.Date[in.cens1p0]=followup.max.date.mm.dd.yyyy[in.cens1p0]
               
               in.cens1p1=
                 ed$Status.at.Death %in% c("Dead of Other") &
                 Date.of.Death.mm.dd.yyyy <= cutoff  & 
                 !in.ev
               ev.status[in.cens1p1]=DSS.CENSOR
               ev.Date[in.cens1p1]=Date.of.Death.mm.dd.yyyy[in.cens1p1]
               
               in.cens2=
                 ed$Status.at.Death %in% c("Dead of Disease") & 
                 Date.of.Death.mm.dd.yyyy > cutoff &
                 !in.ev
               ev.status[in.cens2]=DSS.CENSOR
               ev.Date[in.cens2]=cutoff[in.cens2]
               
               in.cens3=
                 (
                   (ed$Status.at.Last.Contact %in% c("Alive No Evidence of Disease", "Alive Unspecified", "Alive With Disease", "Alive With Morbidity") & followup.max.date.mm.dd.yyyy > cutoff) |
                     (ed$Status.at.Death        %in% c("Dead of Other", "Dead of Intercurrent Disease", "Dead Unspecified")                               & Date.of.Death.mm.dd.yyyy     > cutoff)	
                 ) & 
                 !in.ev
               ev.status[in.cens3]=DSS.CENSOR
               ev.Date[in.cens3]=cutoff[in.cens3]
               
               in.NA=
                 ed$Status.at.Death %in% c("Dead of Intercurrent Disease", "Dead Unspecified") & 
                 Date.of.Death.mm.dd.yyyy <= cutoff
               ev.status[in.NA]=MISSING.UNK
               ev.Date[in.NA]=MISSING.UNK
          },
          RFS={ 
            #First Event Scenario date of recorded recurrence is available and smaller than cutoff
            in.ev1=!is.na(Date.of.First.Recorded.Recurrence.mm.dd.yyyy) & Date.of.First.Recorded.Recurrence.mm.dd.yyyy <= cutoff
            ev.status[in.ev1]=RFS.EVENT
            ev.Date[in.ev1]=Date.of.First.Recorded.Recurrence.mm.dd.yyyy[in.ev1] # return date objects
            
            #Second Scenario date of recorded recurrence is not available but status at death is recurred
            in.ev2=is.na(Date.of.First.Recorded.Recurrence.mm.dd.yyyy) & ed$Status.at.Death%in% c("Dead of Disease")    & followup.start.date.mm.dd.yyyy <= cutoff
            ev.status[in.ev2]=RFS.EVENT
            ev.Date[in.ev2]=NA # still unknown relapse date
            
            #Third Scenario date of recorded recurrence is not available but status at Last Contact is alive with disease
            in.ev3=is.na(Date.of.First.Recorded.Recurrence.mm.dd.yyyy) & ed$Status.at.Last.Contact %in% c("Alive With Disease") & followup.start.date.mm.dd.yyyy <= cutoff
            ev.status[in.ev3]=RFS.EVENT
            ev.Date[in.ev3]=NA # still unknown relapse date
            
            in.ev=in.ev1 | in.ev2 | in.ev3 
            #First Censored Event Scenario date of recorded recurrence is available and larger than cutoff		
            in.cens1 = !is.na(Date.of.First.Recorded.Recurrence.mm.dd.yyyy) & Date.of.First.Recorded.Recurrence.mm.dd.yyyy > cutoff & !in.ev
            ev.status[in.cens1] = RFS.CENSOR
            ev.Date[in.cens1]=cutoff[in.cens1]
            
            #Second Censored Event Scenario date of recorded recurrence is not available and alive or dead of other
            in.cens2 = is.na(Date.of.First.Recorded.Recurrence.mm.dd.yyyy) & 
              ((ed$Status.at.Death        %in% c("Dead of Other")) | (ed$Status.at.Last.Contact %in% c("Alive No Evidence of Disease","Alive With Morbidity") 
                                                                      & ed$Status.at.Death %in% ALL.MISSING.CODES)) & !in.ev
            ev.status[in.cens2] = RFS.CENSOR
            ev.Date[in.cens2] = apply(cbind(followup.end.date.mm.dd.yyyy,cutoff),1,min)[in.cens2]
            
            # case when BOTH status and last contact and status at death are available ...
            in.cens3 =!ed$Status.at.Death %in% ALL.MISSING.CODES & !ed$Status.at.Last.Contact %in% ALL.MISSING.CODES &
              !is.na(followup.max.date.mm.dd.yyyy) & followup.max.date.mm.dd.yyyy > cutoff & 
              ed$Status.at.Last.Contact %in% c("Alive No Evidence of Disease","Alive With Morbidity") & 
              !in.ev
            ev.status[in.cens3] = RFS.CENSOR
            ev.Date[in.cens3] = cutoff[in.cens3]
            
            in.cens4 =
              !ed$Status.at.Death %in% ALL.MISSING.CODES & !ed$Status.at.Last.Contact %in% ALL.MISSING.CODES &
              !is.na(followup.max.date.mm.dd.yyyy) & followup.max.date.mm.dd.yyyy < cutoff &
              ed$Status.at.Death %in% c("Dead of Other") & # no need to look at status at last followup since this is AFTER date of last followup
              !in.ev
            ev.status[in.cens4] = RFS.CENSOR
            ev.Date[in.cens4] = followup.end.date.mm.dd.yyyy[in.cens4] #cutoff[in.cens4]
           
            in.cens5 = # cut off date sandwiched between followup.max/end.date
              !ed$Status.at.Death %in% ALL.MISSING.CODES & !ed$Status.at.Last.Contact %in% ALL.MISSING.CODES &
              !is.na(followup.max.date.mm.dd.yyyy) & !is.na(followup.end.date.mm.dd.yyyy) & followup.max.date.mm.dd.yyyy <= cutoff & followup.end.date.mm.dd.yyyy > cutoff &
              ed$Status.at.Death %in% c("Dead of Other") & # no need to look at status at last followup since this is AFTER date of last followup
              !in.ev	
            ev.status[in.cens5] = RFS.CENSOR
            ev.Date[in.cens5] = cutoff[in.cens5]
            # all other cases would be rfs unknown date/status
            # all other cases would be rfs unknown date/status
          }
  ) # switch
  
  # 'remove' all case with cutoff < followup start date
  before.fu <- followup.start.date.mm.dd.yyyy > cutoff
  ev.status[before.fu]=MISSING.UNK
  ev.Date[before.fu]=MISSING.UNK
  
  # the cases that were lost to following ... always N/A regardless of followup cut off
  in.NA=ed$followup.lost=="Yes"
  ev.status[in.NA]=MISSING.UNK
  ev.Date[in.NA]=MISSING.UNK
  if (status.only) {
    return(ev.status)
  } else {
    return(list(
      "ev.status"=ev.status,
	  "ev.Date"=as.Date(ev.Date, origin="1970-01-01"),
      "ev.years"=as.numeric((as.Date(ev.Date, origin="1970-01-01")-followup.start.date.mm.dd.yyyy)/365.24)
    ))
  }
}

