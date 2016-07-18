#' Build cutpoint variables
#' 
#' Transforms an ordinal variable into anywhere from two to five groups for cutpoint analysis.
#'
#' @param x vector to split by certain cutpoints
#' @param n either "b" for binarization (2 groups), "t" for trinarization (3 groups),
#' "q" for quads (4 groups), or "qn" for quints (5 groups)
#' @param var.prefix variable name prefix
#' @param list if \code{TRUE}, the variables are returned as a list.
#'
#' @return By default, a data frame of cutpoint variables built from a categorical biomarker. The
#' number of columns correspond to all the ways the biomarker could be cut into \code{n}
#' bins. Each column name starts with a "b", "t", "qd", or "qn" for "binarization", "trinarization",
#' "quads", or "quints", respectively, with the levels being compared separated by "v". If \code{list = FALSE},
#' each cutpoint variable is an element of a list.
#' 
#' @author Derek Chiu
#' @export
#'
#' @examples
#' set.seed(1108)
#' x <- sample(0:4, size = 1000, replace = TRUE)
#' head(build_cuts(x, n = "b"))
#' head(build_cuts(x, n = "t"))
#' head(build_cuts(x, n = "t", var.prefix = "PHGDH"))
#' str(build_cuts(x, n = "qd", list = TRUE))
build_cuts <- function(x, n = c("b", "t", "qd", "qn"), var.prefix = NULL,
                       list = FALSE) {
  . <- NULL
  n <- match.arg(n)
  ulevs <- sort(unique(x[x > min(x)]))
  if (n == "b") {
    cuts <- rbind(ulevs, max(x))
  } else if (n == "t") {
    cuts <- rbind(combn(ulevs, 2), max(x))
  } else if (n == "qd") {
    cuts <- rbind(combn(ulevs, 3), max(x))
  } else if (n == "qn") {
    cits <- rbind(combn(ulevs, 4), max(x))
  }
  cut.list <- plyr::alply(cuts, 2, Hmisc::cut2, x = x)
  v.name <- cut.list %>% 
    sapply(function(z) z %>% 
             table() %>% 
             names() %>% 
             stringr::str_replace_all("[\\[,\\)\\]]", "") %>% 
             sapply(function(z) {
               y <- as.numeric(unlist(strsplit(z, "")))
               if (length(y) > 1) {
                 return(paste(seq(y[1], y[2] - 1), collapse = ""))
               } else {
                 return(y)
               }
             }) %>% 
             paste(collapse = "v") %>% 
             paste0(n, .) %>% 
             ifelse(stringr::str_sub(., -1) == max(x), ., paste0(., max(x))) %>% 
             ifelse(is.null(var.prefix), ., paste0(var.prefix, "_", .))
    )
  result <- data.frame(cut.list) %>% 
    magrittr::set_names(v.name)
  if (list)
    return(as.list(result))
  else
    return(result)
}
