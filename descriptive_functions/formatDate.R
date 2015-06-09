formatDate <- function(day, month, year, date.format = "MM.DD.YYYY", sep = "/") {
  if (date.format == "MM.DD.YYYY") {
    return(paste(sprintf("%02d", month), sprintf("%02d", day), year, sep = sep))
  } else if (date.format == "DD.MM.YYYY") {
    return(paste(sprintf("%02d", day), sprintf("%02d", month), year, sep = sep))
  } else if (date.format == "YYYY.MM.DD") {
    return(paste(year, sprintf("%02d", month), sprintf("%02d", day), sep = sep))
  } else {
    stop('unknown format: "', date.format, '". Use one of "MM.DD.YYYY", "DD.MM.YYYY", "YYYY.MM.DD".')
    return(NA)
  }
}