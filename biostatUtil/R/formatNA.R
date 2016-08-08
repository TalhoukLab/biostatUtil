#' Missing Value Formatting
#' 
#' Takes a numeric vector and replaces all missing codes with NA and returns a
#' factor if the variable is categorical or a numeric variable if it's numeric.
#' 
#' @param y a vector.
#' @param type whether the variable is \code{"cat"} (categorical) or
#'   \code{"cont"} (continuous). Defaults to \code{"cat"}.
#' @param codes vector of missing codes to replace with \code{NA}
#' @return A categorical or numerical vector with all missing formatted as
#'   \code{NA}.
#' @author Aline Talhouk, Derek Chiu
#' @export
#' 
#' @examples 
#' y <- c(1:10, "Unk", 12)
#' formatNA(y)
formatNA <- function(y, type = c("cat", "cont"), codes = c("", "Unk", "N/A")) {
  y[y %in% c(codes, NA)] <- NA
  res <- switch(match.arg(type), cat = factor(y), cont = as.numeric(y))
  return(res)
}