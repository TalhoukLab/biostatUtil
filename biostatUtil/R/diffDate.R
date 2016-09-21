#' Difference between two dates
#' 
#' Takes the difference between two dates and returns in the specified unit of 
#' time.
#' 
#' The unit of time can be \code{"days"} (default), \code{"weeks"},
#' \code{"months"}, or \code{"years"}.
#' 
#' @param d1 first date
#' @param d2 second date
#' @param date.format how to format the resulting date
#' @param units the unit of time for which to take the difference. Defaults to 
#'   "days".
#' @param existing.missing.codes missing dates
#' @param return.missing.code what to return if there is a missing input
#' @param ... additional arguments to \code{formatDate}
#' @return The difference between two dates \code{d1 - d2} returned in the 
#'   specified unit of time.
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples
#' ## Wrong Order
#' diffDate("1992/01/27", "2003/03/21", date.format = "YYYY.MM.DD")
#' 
#' ## Later date comes first, subtracts the second date
#' diffDate("2003/03/21", "1992/01/27", date.format = "YYYY.MM.DD")
#' 
#' ## Different separator
#' diffDate("2003-03-21", "1992-01-27", date.format = "YYYY.MM.DD", sep = "-")
diffDate <- function(d1, d2, date.format = "MM.DD.YYYY",
                     units = c("days", "weeks", "months", "years"),
                     existing.missing.codes = NA, return.missing.code = NA,
                     ...) {
  if (is.na(d1) | is.na(d2)) return(NA)
  if (n_distinct(existing.missing.codes, na.rm = TRUE) > 0 &
      any(c(d1, d2) %in% existing.missing.codes))
    return(return.missing.code)
  dates <- lapply(
    c(d1, d2), function(x)
      strptime(cleanDate(x, date.format, date.format,
                         existing.missing.codes = existing.missing.codes,
                         return.missing.code = return.missing.code, ...),
               format = getFormat(x, date.format))
  )
  result <- as.numeric(do.call(difftime, dates))
  switch(match.arg(units),
         days = result,
         weeks = result / 7,
         months = result / NUM.DAYS.IN.MONTH,
         years = result / NUM.DAYS.IN.YEAR)
}