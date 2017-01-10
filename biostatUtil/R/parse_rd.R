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
#'   
#' @return a table with information on a package's documented objects (functions
#'   and data). Rows are objects, and columns are tags.
#' @author Derek Chiu
#' @export
#' 
#' @examples
#' \dontrun{
#' tab <- parse_rd()
#' str(tab)
#' }
parse_rd <- function(path = NULL, tags = c("name", "title", "desc",
                                           "details")) {
  rd.files <- list.files("man", full.names = TRUE)
  rd.parsed <- sapply(rd.files, Rd2roxygen::parse_file)
  info.table <- sapply(rd.parsed, function(x)
    replace(x[tags], sapply(x[tags], is.null), "")) %>% 
    t() %>%
    data.frame() %>%
    sapply(unlist) %>%
    data.frame(stringsAsFactors = FALSE)
  if (!is.null(path)) readr::write_csv(info.table, path = path)
  return(info.table)
}