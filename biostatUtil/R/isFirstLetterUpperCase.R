#' Is the first letter uppercase?
isFirstLetterUpperCase <- function(x) {
  arr <- strsplit(x,"")[[1]]
  if (length(arr) == 0) {
    return(TRUE) # empty string - returns TRUE
  } else {
    return(arr[1] == toupper(arr[1]))
  }
}