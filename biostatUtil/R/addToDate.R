#' Add to a date
#' 
#' Adds a period of time to a date string and returns a new date string.
#' 
#' The added period of time can be in \code{"days"}, \code{"months"},
#' or \code{"years"}. If \code{delta} is negative, then the returned date
#' will be earlier than \code{org.date}. The output date format will be the
#' same as the input date format.
#' 
#' @param org.date original date
#' @param delta amount of time to add to \code{org.date}
#' @param date.format how to format the resulting date
#' @param units the unit of time for which to take the difference. Defaults to "days".
#' @param existing.missing.codes missing dates
#' @param return.missing.code what to return if there is a missing input
#' @return The new date after adding \code{delta} to \code{org.date}.
#' @author Samuel Leung
#' @export
#' @examples
#' addtoDate("2014/07/08", 10, date.format = "YYYY.MM.DD")
#' addtoDate("2014/07/08", 10, date.format = "YYYY.MM.DD", units = "months")
#' addtoDate("2014/07/08", -10, date.format = "YYYY.MM.DD", units = "years")
addtoDate <- function(org.date, delta, date.format = "MM.DD.YYYY",
                      units = "days", existing.missing.codes = NA,
                      return.missing.code = NA) {
  if (is.na(org.date) | is.na(delta)) 
    return(NA)
  if (length(unique(existing.missing.codes
                    [!is.na(existing.missing.codes)])) > 0 &
      (org.date %in% existing.missing.codes | delta %in% existing.missing.codes))
    return(return.missing.code)
  delta <- as.numeric(delta)
  delta.in.days <- delta
  if (units == "weeks") {
    delta.in.days <- delta * 7
  } else if (units == "months") {
    delta.in.days <- delta * NUM.DAYS.IN.MONTH
  } else if (units == "years") {
    delta.in.days <- delta * NUM.DAYS.IN.YEAR
  }
  return(format(as.Date((as.Date(
    cleanDate(org.date, date.format, date.format,
              existing.missing.codes = existing.missing.codes,
              return.missing.code = return.missing.code),
    format = getFormat(org.date, date.format), origin = DATE.ORIGIN) + delta.in.days),
    origin = DATE.ORIGIN), format = getFormat(org.date, date.format)))
}