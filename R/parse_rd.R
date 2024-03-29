#' Parse all Rd files in package
#'
#' Parse all Rd files in a package and return metadata as a data frame.
#'
#' The function requires that the working directory is set to the package's root
#' directory.
#'
#' @param file file path to save table
#' @param tags sections to extract from Rd files. Missing entries labelled as
#'   `NA`
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
parse_rd <- function(file = NULL, tags = c("name", "title", "desc",
                                           "details")) {
  if (!requireNamespace("Rd2roxygen", quietly = TRUE)) {
    stop("Package \"Rd2roxygen\" is required. Please install it.",
         call. = FALSE)
  }
  rd.files <- list.files("man", full.names = TRUE)
  info.table <- rd.files %>%
    purrr::set_names() %>%
    purrr::map(Rd2roxygen::parse_file) %>%
    purrr::map(~ replace(.x[tags], purrr::map_lgl(.x[tags], is.null), "")) %>%
    purrr::transpose() %>%
    purrr::map(unlist) %>%
    data.frame()
  if (!is.null(file)) readr::write_csv(info.table, file = file)
  info.table
}
