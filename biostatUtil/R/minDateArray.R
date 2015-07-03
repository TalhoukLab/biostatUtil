#' Minimum date of an array
#' @export
minDateArray <- function(t.arr, date.format = "MM.DD.YYYY",
                    existing.missing.codes = NA, return.missing.code = NA) {
  t.arr <- t.arr[!is.na(t.arr)]
  if (length(unique(existing.missing.codes
                    [!is.na(existing.missing.codes)])) > 0) {
    t.arr <- t.arr[!(t.arr %in% existing.missing.codes)]
  }
  if (length(t.arr) == 0) {
    return(return.missing.code)
  }
  return(format(as.Date(min(sapply(t.arr, function(x){
    return(
      as.Date(
        cleanDate(x, date.format, date.format,
                  existing.missing.codes = existing.missing.codes,
                  return.missing.code = return.missing.code),
        format = date.format, origin = DATE.ORIGIN))
  }, USE.NAMES = FALSE)), origin = DATE.ORIGIN), format = date.format))
}