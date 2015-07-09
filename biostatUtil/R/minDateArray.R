#' Minimum of date array
#' 
#' Finds the minimum (earliest) date in an array of date strings.
#' 
#' @param t.arr array of date strings
#' @param date.format format of the array of dates
#' @param existing.missing.codes missing dates
#' @param return.missing.code what to return if there is a missing input
#' @param ... additional arguments to \code{formatDate}
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples 
#' many.dates <- c("03/21/1992", "04/21/2013", "10/10/2015")
#' minDateArray(many.dates)
#' 
#' many.dates <- c("2009-03-01", "2010-01-12", "2015-01-11")
#' minDateArray(many.dates, sep = "-")
#' 
#' ties.dates <- c("2009-03-01", "2009-03-11", "2010-01-12")
#' minDateArray(ties.dates, sep = "-")
minDateArray <- function(t.arr, date.format = "MM.DD.YYYY",
                         existing.missing.codes = NA,
                         return.missing.code = NA, ...) {
  t.arr <- t.arr[!is.na(t.arr)]
  if (length(unique(existing.missing.codes
                    [!is.na(existing.missing.codes)])) > 0) {
    t.arr <- t.arr[!(t.arr %in% existing.missing.codes)]
  }
  if (length(t.arr) == 0) {
    return(return.missing.code)
  }
  min.index <- which.min(sapply(t.arr, function(x){
    as.Date(
      cleanDate(x, date.format, date.format,
                existing.missing.codes = existing.missing.codes,
                return.missing.code = return.missing.code, ...),
      format = getFormat(x, date.format), origin = DATE.ORIGIN)
  }, USE.NAMES = FALSE))
  return(t.arr[min.index])
}