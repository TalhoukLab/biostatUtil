#' Wrap list of `ggkm` plots into a single patchwork
#'
#' Kaplan-Meier plots generated for multiple survival outcomes can be wrapped
#' into a single figure.
#'
#' @param x list of `ggkm()` figures
#' @param ... additional annotation parameters passed to
#'   [patchwork::plot_annotation()]
#' @export
wrap_ggkm <- function(x, ...) {
  x %>%
    patchwork::wrap_plots(ncol = 1) +
    patchwork::plot_annotation(...)
}
