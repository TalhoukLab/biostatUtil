#' Missing Value Formatting Function
#'
#' Takes a numeric vector and replaces all Missing codes with NA and returns a factor if the variable is categorical or a numeric variable if it's numeric.
#' @param y a vector.
#' @param type whether the variable is "cat" or "cont". Defaults to cat.
#' @return A categorical or numerical vector with all missing as formatted NA.
#' @author Aline Talhouk
#' @export
#'
formatNA <- function(y,type="cat"){
  x <- y
  x[x %in% c("", "Unk", "N/A", NA)] <- NA
  if(type == "cat"){
    res <- factor(x)
  }else{
      res <- as.numeric(x)}
return(res)
}

