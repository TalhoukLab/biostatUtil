#' Date computations
#' 
#' \code{addToDate} adds a period of time to a date string and returns a new 
#' date string. \code{diffDate} takes the difference between two dates and 
#' returns in the specified unit of time. \code{compareDate} compares the 
#' difference between two dates.
#' 
#' If \code{delta} is negative, then the returned date will be earlier than 
#' \code{org.date}. The output date format will be the same as the input date 
#' format.
#' 
#' The unit of time to add to or return in can be in \code{"days"}, 
#' \code{"weeks"}, \code{"months"}, or \code{"years"}. \code{compareDate} calls 
#' \code{diffDate} and returns integer values specifying which date is earlier 
#' (or if they are the same). \code{d1} should be later than \code{d2} so the 
#' function returns nonnegative values.
#' 
#' @param org.date original date
#' @param delta amount of time to add to \code{org.date}
#' @param date.format how to format the resulting date
#' @param units for \code{addToDate}, the unit of time for \code{delta}; for 
#'   \code{diffDate}, the unit of time for which to take the difference. 
#'   Defaults to "days".
#' @param existing.missing.codes missing dates
#' @param return.missing.code what to return if there is a missing input
#' @param sep date separator string
#' @return \code{addDate} returns the new date after adding \code{delta} to
#'   \code{org.date}.
#' @name date_compute
#' @family date computation functions
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples
#' ## Adding to a date
#' addToDate("2014/07/08", 10, date.format = "YYYY.MM.DD")
#' addToDate("2014-07-08", 10, date.format = "YYYY.MM.DD", sep = "-")
#' addToDate("2014/07/08", 10, date.format = "YYYY.MM.DD", units = "months")
#' addToDate("2014/07/08", -10, date.format = "YYYY.MM.DD", units = "years")
#' 
#' ## Date differences
#' # Later date comes first, subtracts the second date
#' diffDate("2003/03/21", "1992/01/27", date.format = "YYYY.MM.DD")
#' 
#' # Otherwise negative
#' diffDate("1992/01/27", "2003/03/21", date.format = "YYYY.MM.DD")
#' 
#' # Different separator
#' diffDate("2003-03-21", "1992-01-27", date.format = "YYYY.MM.DD", sep = "-")
#' 
#' ## Date comparisons
#' compareDate("01/22/1949", "04/13/1950", date.format = "MM.DD.YYYY")
#' compareDate("04/13/1950", "04/13/1950", date.format = "MM.DD.YYYY")
#' compareDate("04/13/1959", "04/13/1950", date.format = "MM.DD.YYYY")
#' compareDate("01-22-1949", "04-13-1950", date.format = "MM.DD.YYYY", sep = "-")
addToDate <- function(org.date, delta, date.format = "MM.DD.YYYY",
                      units = c("days", "weeks", "months", "years"),
                      existing.missing.codes = NA, return.missing.code = NA,
                      sep = "/") {
  if (is.na(org.date) | is.na(delta)) 
    return(NA)
  if (length(unique(existing.missing.codes
                    [!is.na(existing.missing.codes)])) > 0 &
      (org.date %in% existing.missing.codes |
       delta %in% existing.missing.codes))
    return(return.missing.code)
  delta <- as.numeric(delta)
  delta.time <- switch(match.arg(units),
                       days = delta,
                       weeks = delta * 7,
                       months = delta * NUM.DAYS.IN.MONTH,
                       years = delta * NUM.DAYS.IN.YEAR)
  return(format(as.Date(as.Date(
    cleanDate(org.date, date.format, date.format,
              existing.missing.codes = existing.missing.codes,
              return.missing.code = return.missing.code, sep = sep),
    format = getFormat(org.date, date.format), origin = DATE.ORIGIN) +
      delta.time, origin = DATE.ORIGIN),
    format = getFormat(org.date, date.format)))
}

#' @param d1 later date
#' @param d2 earlier date
#' @inheritParams addToDate
#' @return \code{diffDate} returns the difference between two dates \code{d1 -
#'   d2} in the specified unit of time.
#' @rdname date_compute
#' @family date computation functions
#' @export
diffDate <- function(d1, d2, date.format = "MM.DD.YYYY",
                     units = c("days", "weeks", "months", "years"),
                     existing.missing.codes = NA, return.missing.code = NA,
                     sep = "/") {
  if (is.na(d1) | is.na(d2)) return(NA)
  if (n_distinct(existing.missing.codes, na.rm = TRUE) > 0 &
      any(c(d1, d2) %in% existing.missing.codes))
    return(return.missing.code)
  dates <- lapply(
    c(d1, d2), function(x)
      strptime(cleanDate(x, date.format, date.format,
                         existing.missing.codes = existing.missing.codes,
                         return.missing.code = return.missing.code, sep = sep),
               format = getFormat(x, date.format))
  )
  result <- as.numeric(do.call(difftime, dates))
  switch(match.arg(units),
         days = result,
         weeks = result / 7,
         months = result / NUM.DAYS.IN.MONTH,
         years = result / NUM.DAYS.IN.YEAR)
}

#' @inheritParams diffDate
#' @return \code{compareDate} returns 1 if \code{d1 > d2}, -1 if \code{d1 < d2},
#'   and 0 if \code{d1 == d2}.
#' @rdname date_compute
#' @family date computation functions
#' @export
compareDate <- function(d1, d2, date.format = "MM.DD.YYYY",
                        existing.missing.codes = NA,
                        return.missing.code = NA, sep = "/") {
  difference <- diffDate(d1, d2, date.format = date.format, units = "days",
                         existing.missing.codes = existing.missing.codes,
                         return.missing.code = return.missing.code, sep = sep)
  if (is.na(difference))
    return(return.missing.code)
  if (difference > 0) {
    return(1)
  } else if (difference < 0) {
    return(-1)
  } else {
    return(0)
  }
}