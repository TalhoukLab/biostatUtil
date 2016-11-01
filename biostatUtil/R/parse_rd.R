#' Parse all Rd files in package
#' 
#' Parse all Rd files in a package and return metadata as a data frame.
#' 
#' The function requires that the working directory is set to the package's root
#' directory.
#' 
#' @param path file path to save table
#' @param tags sections to extract from Rd files. Missing entries labelled as
#'   \code{NA}
#' @param save if \code{TRUE}, the table is saved as a csv
#'   
#' @return a table with information on a package's documented objects (functions
#'   and data). Rows are objects, and columns are tags.
#' @author Derek Chiu
#' @export
#' 
#' @examples
#' tab <- parse_rd()
#' str(tab)
parse_rd <- function(path, tags = c("name", "title", "desc", "details"),
                     save = FALSE) {
  rd.files <- list.files("man", full.names = TRUE)
  rd.parsed <- sapply(rd.files, Rd2roxygen::parse_file)
  info.table <- sapply(rd.parsed, function(x) as.list(x[tags])) %>% 
    magrittr::inset(sapply(., is.null), NA) %>% 
    t() %>% 
    as.data.frame() %>% 
    dplyr::mutate_all(unlist)
  if (save) readr::write_csv(info.table, path = path)
  return(info.table)
}