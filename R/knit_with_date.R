#' Knit R markdown report with today's date
#'
#' Append today's date to R markdown output file name. This function modifies
#' the default behaviour of the Knit button in R Studio. It is intended to be
#' passed in the YAML `knit:` field.
#'
#' @param input input file
#' @param sep separator between input file name and date
#' @param ... additional arguments to [rmarkdown::render()]
#' @return The output file name will have today's date appended at the end.
#'
#' @author Derek Chiu
#' @references
#'   \url{https://bookdown.org/yihui/rmarkdown-cookbook/custom-knit.html}
#' @export
knit_with_date <- (function(input, sep = "_", ...) {
  if (!requireNamespace("rmarkdown", quietly = TRUE)) {
    stop("Package \"rmarkdown\" is needed. Please install it.",
         call. = FALSE)
  } else {
    rmarkdown::render(
      input,
      output_file = paste0(tools::file_path_sans_ext(input), sep, Sys.Date()),
      envir = globalenv()
    )
  }
})
