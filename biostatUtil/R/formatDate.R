#' Format a date.
#' 
#' Prints a date into a pretty format.
#' 
#' Given the day, month, and year of a date, returns the date in a specific 
#' format. The order and separating string can be modified using 
#' \code{date.format} and \code{sep} respectively. Take note the order of the 
#' arguments: day, month, and year. Only accepts "MM.DD.YYYY", "MMM.DD.YYYY", 
#' "DD.MM.YYYY", "DD.MMM.YYYY", "YYYY.MM.DD", "YYYY.MMM.DD".
#' 
#' @param d day of the month (1-31)
#' @param m month of the year (1-12)
#' @param y year of date
#' @param date.format how to format the date. Defaults to "month/day/year".
#' @param sep string used to separate \code{d}, \code{m}, and \code{y}. Defaults
#'   to "/".
#' @return A character string of a formatted date.
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples
#' formatDate(8, 7, 2011)
#' formatDate(8, 7, 2011, date.format = "YYYY.MM.DD")
#' formatDate(8, 7, 2011, date.format = "DD.MM.YYYY", sep = "-")
#' formatDate(10, 1, 2015, date.format = "MMM.DD.YYYY", sep = "-")
formatDate <- function(d, m, y, date.format = "MM.DD.YYYY", sep = "/") {
  d <- as.numeric(d)
  m <- as.numeric(m)
  y <- as.numeric(y)
  if (date.format == "MM.DD.YYYY") {
    return(paste(sprintf("%02d", m), sprintf("%02d", d), y, sep = sep))
  } else if (date.format == "MMM.DD.YYYY") {
    return(paste(month.abb[m], sprintf("%02d", d), y, sep = sep))
  } else if (date.format == "DD.MM.YYYY") {
    return(paste(sprintf("%02d", d), sprintf("%02d", m), y, sep = sep))
  } else if (date.format == "DD.MMM.YYYY") {
    return(paste(sprintf("%02d", d), month.abb[m], y, sep = sep))
  } else if (date.format == "YYYY.MM.DD") {
    return(paste(y, sprintf("%02d", m), sprintf("%02d", d), sep = sep))
  } else if (date.format == "YYYY.MMM.DD") {
    return(paste(y, month.abb[m], sprintf("%02d", d), sep = sep))
  } else {
    stop('unknown format: "', date.format, '". Use one of "MM.DD.YYYY",
          "MMM.DD.YYYY", "DD.MM.YYYY", "DD.MMM.YYYY", "YYYY.MM.DD",
          "YYYY.MMM.DD".')
    return(NA)
  }
}