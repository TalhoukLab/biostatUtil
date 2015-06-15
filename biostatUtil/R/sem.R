#' Standard error of the mean
sem <- function(x, missing.value = NA, return.missing.value = NA) { 
  # remove missing values ...
  x <- x[!is.na(x)]
  if (!is.na(missing.value)) {
    x <- x[!x%in%missing.value]
  }
  return(ifelse(
    length(x) == 0,
    return.missing.value,
    sqrt(var(x) / length(x))
  ))
}