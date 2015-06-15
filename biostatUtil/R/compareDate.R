#' Compare two dates
#' @export
compareDate <- function(d1, d2, date.format = MM.DD.YYYY, units = 'days', existing.missing.codes = NA, return.missing.code = NA) {
  difference <- diffDate(d1, d2, date.format = date.format, units = units, existing.missing.codes = existing.missing.codes, return.missing.code = return.missing.code)
  if (is.na(return.missing.code)) {
    if (is.na(difference)) {return(NA)}
  } else {
    if (difference == return.missing.code) {return(return.missing.code)}
  }
  if (difference > 0) {
    return(1)
  } else if (difference < 0) {
    return(-1)
  } else {
    return(0)
  }
}