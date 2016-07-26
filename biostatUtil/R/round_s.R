round_s <- function(x, digits, sci) {
  if (is.na(x)) {
    return(NA)
  } else {
    if (x <= 5 * 10 ^ -(digits + 1)) {
      return(paste0("<", format(1 * 10 ^ -digits, scientific = sci)))
    } else {
      return(round(x, digits))
    }
  }
}