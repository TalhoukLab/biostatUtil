#' Wrap list of `ggkm` plots into a single patchwork
#'
#' Kaplan-Meier plots generated for multiple survival outcomes can be wrapped
#' into a single figure.
#'
#' @param x list of `ggkm()` figures
#' @param ncol number of columns to wrap plots into. Default is 1.
#' @param nrow number of rows to wrap plots into.
#' @param ... additional annotation parameters passed to
#'   [patchwork::plot_annotation()]
#' @export
wrap_ggkm <- function(x, ncol = 1, nrow = NULL, ...) {
  x |>
    patchwork::wrap_plots(ncol = ncol, nrow = nrow) +
    patchwork::plot_annotation(...)
}
