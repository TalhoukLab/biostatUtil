minDate <- function(dates, na.rm = TRUE) {
  if(na.rm)
    dates <- dates[!is.na(dates)]

  if (sum(is.na(dates)) >= 1)
    return(NA) # there is NA
  
  # assume no NA
  min.date <- dates[1]
  for (x in dates) {
    if (x < min.date)
      min.date <- x
  }
  return(min.date)
}