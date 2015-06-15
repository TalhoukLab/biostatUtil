#' Change numeric to date
#' @export
numericToDate <- function(x, date.origin = as.Date("1970-01-01")) {
  return(as.Date(x, origin = date.origin))
}