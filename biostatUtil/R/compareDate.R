#' Compare two dates
#' 
#' Compares the difference between two dates.
#' 
#' Calls \code{diffDate} and returns integer values specifying which date is earlier 
#' (or if they are the same).
#' 
#' @param d1 first date
#' @param d2 second date
#' @param date.format how to format the resulting date
#' @param units the unit of time for which to take the difference. Defaults to "days".
#' @param existing.missing.codes missing dates
#' @param return.missing.code what to return if there is a missing input
#' @param ... additional arguments to \code{formatDate}
#' @return Returns 1 if \code{d1 > d2}, -1 if \code{d1 < d2}, and 0 if \code{d1 == d2}.
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples 
#' compareDate("01/22/1949", "04/13/1950", date.format = "MM.DD.YYYY")
#' compareDate("04/13/1950", "04/13/1950", date.format = "MM.DD.YYYY")
#' compareDate("04/13/1959", "04/13/1950", date.format = "MM.DD.YYYY")
#' compareDate("01-22-1949", "04-13-1950", date.format = "MM.DD.YYYY", sep = "-")
compareDate <- function(d1, d2, date.format = "MM.DD.YYYY", units = "days",
                        existing.missing.codes = NA,
                        return.missing.code = NA, ...) {
  difference <- diffDate(d1, d2, date.format = date.format, units = units,
                         existing.missing.codes = existing.missing.codes,
                         return.missing.code = return.missing.code, ...)
  if (is.na(difference)) {
    return(return.missing.code)
  }
  if (difference > 0) {
    return(1)
  } else if (difference < 0) {
    return(-1)
  } else {
    return(0)
  }
}