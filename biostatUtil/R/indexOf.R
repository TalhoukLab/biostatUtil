#' Find occurences of string within another stirng
indexOf <- function(a, b, ignore.case = FALSE) {
  if (ignore.case) {
    a <- toupper(a)
    b <- toupper(b)
  }
  b.len <- nchar(b)
  a.len <- nchar(a)
  if (b.len>a.len) {
    return(NA) # b is longer than a, therefore, not possible for b to be found in a
  }
  a.arr <- strsplit(a,"")[[1]]
  indexes <- c()
  i <- 1
  while (i <= (a.len-b.len+1)) {
    if  (paste(a.arr[i:(i+b.len-1)],collapse="")==b) {
      indexes <- c(indexes,i)
      i <- i+b.len-1
    }
    i <- i+1
  }
  if (length(indexes)>0) {
    return(indexes)
  } else {
    return(NA)
  }
}