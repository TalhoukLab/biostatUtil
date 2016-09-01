#' Earliest Date
#' 
#' Finds the earliest date given a vector of dates.
#' 
#' The input vector should have dates formatted as YYYY-MM-DD. If \code{na.rm} 
#' is not set to the default (\code{TRUE}) and \code{dates} has \code{NA} 
#' values, then the function will also return \code{NA}.
#' 
#' @param dates vector of dates
#' @param na.rm logical. Should NAs be removed?
#' @return The earliest date from a vector of dates.
#' @author Samuel Leung
#' @export
#' @examples
#' ### No NA
#' t1 <- c("2015-03-01", "2015-02-15", "2015-05-01")
#' minDate(t1)
#' 
#' ### With NA
#' t2 <- c("2015-03-01", "2015-02-15", NA, "2015-05-01")
#' minDate(t2)
#' minDate(t2, na.rm = FALSE)
minDate <- function(dates, na.rm = TRUE) {
  if(na.rm)
    dates <- dates[!is.na(dates)]

  if (sum(is.na(dates)) >= 1)
    return(NA)
  
  min.date <- dates[1]
  for (x in dates) {
    if (x < min.date)
      min.date <- x
  }
  return(min.date)
}