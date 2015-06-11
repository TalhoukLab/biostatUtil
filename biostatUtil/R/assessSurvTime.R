#' Assess survival time
#' 
#' Given two survival times and censoring status, assesses the survival times...[To Be Completed]
#' 
#' 
#' @param T1 time 1
#' @param T2 time 2
#' @param status censoring status
assessSurvTime <- function(T1, T2, status) { 
  # in case there are any is.na(status)
  # T2 may be NA as well for rfs!!!
  non.missing.cases <- !is.na(status) & !is.na(T2)
  T1 <- T1[non.missing.cases]
  T2 <- T2[non.missing.cases]
  status <- status[non.missing.cases]
  
  Otime <- T2 - T1  # observation time
  Stime <- T2[status] - T1[status]  # censoring time
  Etime <- max(T2) - T1  # time to end of study
  SurvTime <- T2 - T1
  KFT <- SurvTime
  KFT[status] <- T2[status] - T1[status]  # known function time
  rev.status <- rep(1,length(status))
  rev.status[status] <- 0
  Ftime <- survival::survfit(Surv(as.numeric(SurvTime), rev.status) ~ 1)
  SumServ <- read.table(textConnection(capture.output(Ftime)), skip = 2, header = TRUE)
  
  MedianTime <- list(
    Otime = as.numeric(round(median(Otime, na.rm = T) / 365.24, 2)),
    Stime = as.numeric(round(median(Stime, na.rm = T) / 365.24, 2)),
    Etime = as.numeric(round(median(Etime, na.rm = T) / 365.24, 2)),
    KFT = as.numeric(round(median(KFT, na.rm = T) / 365.24, 2)), 
    RevKM = as.numeric(round(SumServ[, "median"] / 365.24, 2)))
  return(MedianTime)
}

# do.km.plots.pole
# do.km.plots.pole.x.init.treatment
# testInter
# printInterModels
# coxphOut
# printCoxMod
# do.check.ph