#' Build cutpoint variables
#' 
#' Transforms an ordinal variable into two, three, or four groups for cutpoint analysis.
#'
#' @param x vector to split by certain cutpoints
#' @param n either "b" for binarization (2 groups), "t" for trinarization (3 groups), or
#' "q" for separating into quads (4 groups)
#' @param var.prefix variable name prefix
#'
#' @return A data frame of cutpoint variables built from a categorical biomarker. The
#' number of columns correspond to all the ways the biomarker could be cut into \code{n}
#' bins. Each column name starts with a "b", "t", or "q" for "binarization", "trinarization",
#' or "quads", respectively, with the levels being compared separated by "v".
#' @author Derek Chiu
#' @export
#'
#' @examples
#' set.seed(1108)
#' x <- sample(0:4, size = 1000, replace = TRUE)
#' head(build_cuts(x, n = "b"))
#' head(build_cuts(x, n = "t"))
#' head(build_cuts(x, n = "t", var.prefix = "PHGDH"))
#' head(build_cuts(x, n = "q"))
build_cuts <- function(x, n = c("b", "t", "q"), var.prefix = NULL) {
  . <- NULL
  n <- match.arg(n)
  ulevs <- sort(unique(x[x > min(x)]))
  if (n == "b") {
    cuts <- rbind(ulevs, max(x))
  } else if (n == "t") {
    cuts <- rbind(combn(ulevs, 2), max(x))
  } else if (n == "q") {
    cuts <- rbind(combn(ulevs, 3), max(x))
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
  return(result)
}
