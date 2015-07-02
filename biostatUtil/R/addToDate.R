#' Add to a date
#' @param org.date original date
#' @param delta amount of time to add to \code{org.date}
#' @param date.format how to format the resulting date
#' @param units the unit of time for which to take the difference. Defaults to "days".
#' @param existing.missing.codes missing dates
#' @param return.missing.code what to return if there is a missing input
#' @author Samuel Leung
#' @export
addtoDate <- function(org.date, delta, date.format = "MM.DD.YYYY",
                      units = "days", existing.missing.codes = NA,
                      return.missing.code = NA) {
  if (is.na(org.date) | is.na(delta)) 
    return(NA)
  if (length(unique(existing.missing.codes
                    [!is.na(existing.missing.codes)])) > 0 &
      (d1 %in% existing.missing.codes | d2 %in% existing.missing.codes))
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