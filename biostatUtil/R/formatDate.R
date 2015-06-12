#' Format a date.
#' 
#' Prints a date into a pretty format.
#' 
#' Given the day, month, and year of a date, returns the date in a specific format. 
#' The order and separating string can be modified using \code{date.format} and \code{sep} respectively.
#' Take note the order of the arguments: day, month, and year. The current version
#' only accepts "MM.DD.YYYY", "DD.MM.YYYY", "YYYY.MM.DD".
#' 
#' @param d day of the month (1-31)
#' @param m month of the year (1-12)
#' @param y year of date
#' @param date.format how to format the date. Defaults to "month/day/year".
#' @param sep string used to separate \code{d}, \code{m}, and \code{y}. Defaults to "/".
#' @return A character string of a formatted date.
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples
#' formatDate(8, 7, 2011)
#' formatDate(8, 7, 2011, date.format = "YYYY.MM.DD")
#' formatDate(8, 7, 2011, date.format = "DD.MM.YYYY", sep = "-")
formatDate <- function(d, m, y, date.format = "MM.DD.YYYY", sep = "/") {
  if (date.format == "MM.DD.YYYY") {
    return(paste(sprintf("%02d", m), sprintf("%02d", d), y, sep = sep))
  } else if (date.format == "DD.MM.YYYY") {
    return(paste(sprintf("%02d", d), sprintf("%02d", m), y, sep = sep))
  } else if (date.format == "YYYY.MM.DD") {
    return(paste(y, sprintf("%02d", m), sprintf("%02d", d), sep = sep))
  } else {
    stop('unknown format: "', date.format, '". Use one of "MM.DD.YYYY", "DD.MM.YYYY", "YYYY.MM.DD".')
    return(NA)
  }
}