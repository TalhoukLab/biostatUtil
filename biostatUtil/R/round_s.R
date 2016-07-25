round_s <- function(x, digits) {
  if (is.na(x)) {
    return(NA)
  } else {
    if (x <= 5 * 10 ^ -(digits + 1)) {
      return(paste0("<", 1 * 10 ^ -digits))
    } else {
      return(round(x, digits))
    }
  }
}