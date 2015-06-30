#' Change numeric to date
#' 
#' Change a numeric value to a date object by specifying a date of origin.
#' 
#' @param x a number that represents the number of days after
#' \code{date.origin}
#' @param date.origin the date from which we count the number of days
#' passed
#' @return A date object, converted from a numeric object.
#' @author Samuel Leung
#' @export
#' @examples 
#' numericToDate(10)
#' numericToDate(10, "2000-09-11")
numericToDate <- function(x, date.origin = DATE.ORIGIN) {
  return(as.Date(x, origin = date.origin))
}