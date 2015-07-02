#' Difference between two dates
#' 
#' Takes the difference between two dates and returns in the specified
#' unit of time.
#' 
#' The unit of time can be \code{"days"} (default), \code{"months"}, or \code{"years").}
#' 
#' @param d1 first date
#' @param d2 second date
#' @param date.format how to format the resulting date
#' @param units the unit of time for which to take the difference. Defaults to "days".
#' @param existing.missing.codes missing dates
#' @param return.missing.code what to return if there is a missing input
#' @return The difference between two dates \code{d1 - d2} returned in the specified 
#' unit of time.
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples
#' ## Wrong Order
#' diffDate("1992/01/27", "2003/03/21", date.format = "YYYY.MM.DD")
#' 
#' ## Later date comes first, subtracts the second date
#' diffDate("2003/03/21", "1992/01/27", date.format = "YYYY.MM.DD")
diffDate <- function(d1, d2, date.format = "MM.DD.YYYY", units = "days",
                             existing.missing.codes = NA,
                             return.missing.code = NA) {
  if(is.na(d1) | is.na(d2))
    return(NA)
  if (length(unique(existing.missing.codes
                    [!is.na(existing.missing.codes)])) > 0 &
      (d1 %in% existing.missing.codes | d2 %in% existing.missing.codes))
    return(return.missing.code)
  
  result <- as.numeric(
    difftime(
      strptime(cleanDate(d1, date.format, date.format,
                         existing.missing.codes = existing.missing.codes,
                         return.missing.code = return.missing.code),
               format = getFormat(d1, date.format)),
      strptime(cleanDate(d2, date.format, date.format,
                         existing.missing.codes = existing.missing.codes,
                         return.missing.code = return.missing.code),
               format = getFormat(d2, date.format)),
      units = ifelse(units %in% c("months", "years"), "days", units)))
  if (units == "months") {
    return(result / NUM.DAYS.IN.MONTH)
  } else if (units == "years") {
    return(result / NUM.DAYS.IN.YEAR)
  } else {
    return(result)
  }
}