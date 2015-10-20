#' Clean dates
#' 
#' Clean a date and reformat it to another style.
#' 
#' @param x a date string or a numeric representation of a date
#' (e.g. January 13th, 1991 would be 19910113)
#' @param original.format format of input \code{x}
#' @param preferred.format format to change \code{x} to
#' @param existing.missing.codes missing dates
#' @param return.missing.code what to return if there is a missing input
#' @param ... additional arguments to \code{\link{formatDate}}
#' @return A date string cleaned and formatted from the original (unformatted)
#' date
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples 
#' cleanDate("09/11/1991", original.format = "MM.DD.YYYY", preferred.format = "DD.MM.YYYY")
#' cleanDate(11091991, original.format = "DD.MM.YYYY", preferred.format = "YYYY.MMM.DD")
#' cleanDate(11091991, original.format = "DD.MM.YYYY", preferred.format = "YYYY.MMM.DD", sep = "-")
cleanDate <- function(x, original.format, preferred.format,
                      existing.missing.codes = "", return.missing.code = NA,
                      ...) {
  if (is.na(x))
    return(return.missing.code)
  x <- trimws(x)
  if (x %in% existing.missing.codes)
    return(x)
  date.comp <- strsplit(x, "/|-")[[1]]
  temp <- suppressWarnings(as.numeric(date.comp[1]))
  if (is.na(temp))
    return(return.missing.code)
  if (temp > 1000000) {  # yyyymmdd or ddmmyyyy or mmddyyyy
    temp <- paste0(ifelse(temp < 10000000, "0", ""), temp)  # pad leading 0 for jan-sept; turn temp back to string
    if (original.format == "DD.MM.YYYY") {
      return(formatDate(substr(temp, 1, 2), substr(temp, 3, 4),
                        substr(temp, 5, 8), date.format = preferred.format,
                        ...))
    } else if (original.format == "MM.DD.YYYY") {
      return(formatDate(substr(temp, 3, 4), substr(temp, 1, 2),
                        substr(temp, 5, 8), date.format = preferred.format,
                        ...))
    } else if (original.format == "YYYY.MM.DD") {
      return(formatDate(substr(temp, 7, 8), substr(temp, 5, 6),
                        substr(temp, 1, 4), date.format = preferred.format,
                        ...))
    } else {
      stop('ERROR (cleanDate): original.format must be one of
           "DD.MM.YYYY", "MM.DD.YYYY", or "YYYY.MM.DD".')
      return(NA)
    }
  } else if (temp > 31) {  # must be YYYY/MM/DD
    return(formatDate(date.comp[3], date.comp[2], date.comp[1],
                      date.format = preferred.format, ...))
  } else if (temp > 12) {  # must be DD/MM/YYYY
    return(formatDate(date.comp[1], date.comp[2], date.comp[3],
                      date.format = preferred.format, ...))
  } else { # first component <= 12 ... however, we are not sure if it refers to a day or month
    temp <- as.numeric(date.comp[2]) # second component can either be a day or month
    if (temp > 12) {   # must be MM/DD/YYYY
      return(formatDate(date.comp[2], date.comp[1], date.comp[3],
                        date.format = preferred.format, ...))
    } else {  # BOTH first & second component <=12; assume original.format
      if (original.format == "MM.DD.YYYY") {
        return(formatDate(date.comp[2], date.comp[1], date.comp[3],
                          date.format = preferred.format, ...))
      } else if (original.format == "DD.MM.YYYY") {
        return(formatDate(date.comp[1], date.comp[2], date.comp[3],
                          date.format = preferred.format, ...))
      } else {
        stop('ERROR (cleanDate): unknown original.format specified: ',
             original.format)
        return(NA)
      }
    }
  }
}