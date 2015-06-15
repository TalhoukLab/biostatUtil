#' Difference between two dates
#' @export
diffDate <- function(d1, d2, date.format = MM.DD.YYYY, units = 'days',
                             existing.missing.codes = NA,
                             return.missing.code = NA) {
  if(is.na(d1)) {return(NA)}
  if(is.na(d2)) {return(NA)}
  if (length(unique(existing.missing.codes
                    [!is.na(existing.missing.codes)])) > 0) {
    if (d1 %in% existing.missing.codes) {return(return.missing.code)}
    if (d2 %in% existing.missing.codes) {return(return.missing.code)}
  }
  result <- as.numeric(
    difftime(
      strptime(cleanDate(d1, date.format, date.format, existing.missing.codes = existing.missing.codes, return.missing.code = return.missing.code), format = date.format),
      strptime(cleanDate(d2, date.format, date.format, existing.missing.codes = existing.missing.codes, return.missing.code = return.missing.code), format = date.format),
      units = ifelse(units %in% c("months", "years"), "days", units)))
  if (units == "months") {
    return(result / NUM.DAYS.IN.MONTH)
  } else if (units == "years") {
    return(result / NUM.DAYS.IN.YEAR)
  } else {
    return(result)
  }
}