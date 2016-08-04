#' Name cutpoint variables
#' 
#' Create a name for cutpoint variables based on location of cut and number
#' of groups.
#'
#' @param cut.obj a factor variable returned by \code{cut2}
#'
#' @return A character string representing the cutpoint variable name.
#' @author Derek Chiu
#' @importFrom utils tail
#' @export
#'
#' @examples
#' library(Hmisc)
#' set.seed(1108)
#' x <- sample(0:4, size = 1000, replace = TRUE)
#' c1 <- cut2(x, c(1, 4))
#' c2 <- cut2(x, c(2, 4))
#' name_cuts(c1)
#' name_cuts(c2)
name_cuts <- function(cut.obj) {
  . <- NULL
  n <- switch(as.character(nlevels(cut.obj)),
              "2" = "b", "3" = "t", "4" = "qd", "5" = "qn")
  cut.levs <- cut.obj %>% 
    levels() %>% 
    stringr::str_replace_all("[\\[\\)\\]]", "")
  max.lev <- cut.levs %>% 
    tail(1) %>% 
    stringr::str_split_fixed(",", 2) %>% 
    as.numeric() %>% 
    max(na.rm = TRUE)
  cut.name <- cut.levs %>% 
    stringr::str_replace_all(",", "") %>% 
    sapply(function(cut.obj) {
      y <- as.numeric(unlist(strsplit(cut.obj, "")))
      if (length(y) > 1) {
        return(paste(seq(y[1], y[2] - 1), collapse = ""))
      } else {
        return(y)
      }
    }) %>% 
    paste(collapse = "v") %>% 
    paste0(n, .) %>% 
    ifelse(stringr::str_sub(., -1) == max.lev, ., paste0(., max.lev))
  return(cut.name)
}
