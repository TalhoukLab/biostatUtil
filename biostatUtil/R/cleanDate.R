#' Clean dates
#' @export
cleanDate <- function(x, original.format, preferred.format,
                      existing.missing.codes = "", return.missing.code = NA) {
  if (is.na(x)) {return(return.missing.code)}
  x <- trim.white.spaces(x)
  if (x %in% existing.missing.codes) {return(x)}  # preserve error coding
  # need to break the date string into components 
  date.comp <- strsplit(x, "/|-")[[1]]
  temp <- suppressWarnings(as.numeric(date.comp[1]))
  if (is.na(temp)) {
    #cat("WARNING (clean.date.string.and.format): trying to format a non-standard date string: ",x,"\n")
    return(return.missing.code)
  }
  if (temp > 1000000) {
    # this would a format of yyyymmdd or ddmmyyyy or mmddyyyy
    temp <- paste(ifelse(temp < 10000000, "0", ""), temp, sep = "")  # 7 or 8-digits; turn temp back to string
    if (as.numeric(substr(temp, 5, 6)) < 13) {  # assume we will NOT be looking at any dates from 1200's!!!!
      # must be YYYYMMDD
      return(formatDate(substr(temp, 7, 8), substr(temp, 5, 6),
                        substr(temp, 1, 4), date.format = preferred.format))
    } else if (as.numeric(substr(temp, 1, 2)) > 12) {
      # must be DDMMYYYY
      return(formatDate(substr(temp, 1, 2), substr(temp, 3, 4), substr(temp, 5, 8), date.format = preferred.format))
    } else if (as.numeric(substr(temp, 3, 4)) > 12) {
      # must be MMDDYYYY
      return(formatDate(substr(temp, 3, 4), substr(temp, 1, 2), substr(temp, 5, 8), date.format = preferred.format))
    } else if (original.format == DDMMYYYY) {
      return(formatDate(substr(temp, 1, 2), substr(temp, 3, 4), substr(temp, 5, 8), date.format = preferred.format))
    } else {
      # assume must be MMDDYYYY
      return(formatDate(substr(temp, 3, 4), substr(temp, 1, 2), substr(temp, 5, 8), date.format = preferred.format))
    }
  } else if (temp > 31) {
    # must be YYYY/MM/DD because no day can be > 31
    return(formatDate(date.comp[3], date.comp[2], date.comp[1], date.format = preferred.format))
  } else if (temp > 12) {
    # must be DD/MM/YYYY
    return(formatDate(date.comp[1], date.comp[2], date.comp[3], date.format = preferred.format))
  } else { # first component is <= 12 ... however, we are not sure if it refers to a day or month
    temp <- as.numeric(date.comp[2]) # second component can either be a day or month
    if (temp > 12) {
      # must be MM/DD/YYYY
      return(formatDate(date.comp[2], date.comp[1], date.comp[3], date.format = preferred.format))
    } else { # BOTH first and second component is <=12
      # assume its in the original format
      if (original.format == MM.DD.YYYY) {
        return(formatDate(date.comp[2], date.comp[1], date.comp[3], date.format = preferred.format))
      } else if (original.format == DD.MM.YYYY) {
        return(formatDate(date.comp[1], date.comp[2], date.comp[3], date.format = preferred.format))
      } else {
        cat("ERROR (clean.date.string.and.format): unknown original.format specified: ",original.format,"\n")
        return(NA)
      }
    }
  }
}