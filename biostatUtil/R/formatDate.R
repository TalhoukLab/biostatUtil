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
formatDate <- function(d, m, y, date.format = c("MM.DD.YYYY", "MMM.DD.YYYY",
                                                "DD.MM.YYYY", "DD.MMM.YYYY",
                                                "YYYY.MM.DD", "YYYY.MMM.DD"),
                       sep = "/") {
  d <- as.numeric(d)
  m <- as.numeric(m)
  y <- as.numeric(y)
  switch(match.arg(date.format),
         MM.DD.YYYY = paste(sprintf("%02d", m), sprintf("%02d", d), y, sep = sep),
         MMM.DD.YYYY = paste(month.abb[m], sprintf("%02d", d), y, sep = sep),
         DD.MM.YYYY = paste(sprintf("%02d", d), sprintf("%02d", m), y, sep = sep),
         DD.MMM.YYYY = paste(sprintf("%02d", d), month.abb[m], y, sep = sep),
         YYYY.MM.DD = paste(y, sprintf("%02d", m), sprintf("%02d", d), sep = sep),
         YYYY.MMM.DD = paste(y, month.abb[m], sprintf("%02d", d), sep = sep))
}