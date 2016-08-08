#' Calculates ratio with specifying missing values
#' 
#' Returns the ratio of input vectors: x/y with the ability to specify which
#' values are missing
#' @param x input vector
#' @param y input vector
#' @param missing.value missing values in \code{x}
#' @param return.missing.value character to return for missing values
#' @note \code{NAs} are ignored.
#' @author Samuel Leung
#' @export
#' @examples 
#' x <- c(10:1)
#' y <- c(1:10)
#' ratioWithMissing(x, y, c(1:2))
#' 
#' ## All missing
#' ratioWithMissing(x, y, c(1:10))
#' ratioWithMissing(x, y, c(1:10), return.missing.value = "all missing")
ratioWithMissing <- function(x, y, missing.value = -1, return.missing.value = -1) {
	x[x %in% missing.value] <- NA
	y[y %in% missing.value] <- NA
	ratio <- as.numeric(x) / as.numeric(y)
	ratio[is.na(ratio)] <- return.missing.value
	return(ratio)
}