#' Add to a date
#' @export
addtoDate <- function(org.date, delta, date.format = MM.DD.YYYY,
                      units = 'days', existing.missing.codes = NA,
                      return.missing.code = NA) {
  if (is.na(org.date)) {return(NA)}
  if (is.na(delta))    {return(NA)}
  if (length(unique(existing.missing.codes
                    [!is.na(existing.missing.codes)])) > 0) {
    if (org.date %in% existing.missing.codes) {return(return.missing.code)}
    if (delta    %in% existing.missing.codes) {return(return.missing.code)}
  }
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
    format = date.format, origin = DATE.ORIGIN) + delta.in.days),
    origin = DATE.ORIGIN), format = date.format))
}