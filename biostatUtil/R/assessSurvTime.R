#' Assess survival time
#' 
#' Given range of survival times and censoring status, provides different
#' time-related summary statistics
#' 
#' The observation time is defined as the median time in days between all
#' \code{T1} and \code{T2}. Censoring time is the median time in days between
#' all \code{T1} and \code{T2} for events only.
#' 
#' @param T1 vector of start dates
#' @param T2 vector of end dates
#' @param status logical; 
#' @return A list with elements
#' \item{Otime}{Observation time}
#' \item{Stime}{Censoring time}
#' \item{Etime}{Time to end of study}
#' \item{KFT}{Known Function Time}
#' \item{RevKM}{Kaplan-Meier Time}
#' @export
assessSurvTime <- function(T1, T2, status) { 
  # in case there are any is.na(status)
  # T2 may be NA as well for rfs!!!
  non.missing.cases <- !is.na(status) & !is.na(T2)
  T1 <- T1[non.missing.cases]
  T2 <- T2[non.missing.cases]
  status <- status[non.missing.cases]
  
  Otime <- T2 - T1
  Stime <- T2[status] - T1[status]
  Etime <- max(T2) - T1
  SurvTime <- T2 - T1
  KFT <- SurvTime
  KFT[status] <- T2[status] - T1[status]
  rev.status <- rep(1,length(status))
  rev.status[status] <- 0
  Ftime <- survival::survfit(survival::Surv(as.numeric(SurvTime), rev.status) ~ 1)
  SumServ <- read.table(textConnection(capture.output(Ftime)), skip = 2, header = TRUE)
  
  MedianTime <- list(
    Otime = as.numeric(round(median(Otime, na.rm = T) / 365.24, 2)),
    Stime = as.numeric(round(median(Stime, na.rm = T) / 365.24, 2)),
    Etime = as.numeric(round(median(Etime, na.rm = T) / 365.24, 2)),
    KFT = as.numeric(round(median(KFT, na.rm = T) / 365.24, 2)), 
    RevKM = as.numeric(round(SumServ[, "median"] / 365.24, 2)))
  return(MedianTime)
}