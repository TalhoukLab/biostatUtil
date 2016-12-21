#' Plots for MS analyses
#' 
#' \code{ms_boxplot} shows boxplots of different MS expression data values. 
#' \code{ms_mean_var} shows mean-sd plots for the VSN data values to analyze the
#' mean-variance relationship.
#' 
#' @param x data object returned by \code{ms_process}
#' @param width width of plot
#' @param height height of plot
#' @param path file path to save figure. Device is pdf.
#' @return Both functions return a pdf saved to the file location specified by 
#'   \code{path}. \code{ms_boxplot} shows three boxplots of expression values: 
#'   raw data values, log2 and vsn transformed values. \code{ms_mean_var} shows 
#'   the vsn transformed values and mean-sd plots for each treatment group.
#' @name ms_plot
#' @family Mass Spectrometry functions
#' @author Derek Chiu
#' @export
ms_boxplot <- function(x, width = 8, height = 10, path = NULL) {
  dat.plot <- lapply(x[c("raw", "l2", "vsn")], function(y)
    tidyr::gather(as.data.frame(y), key = "Sample", value = "Expression"))
  all.plots <- Map(ms_gg_boxplot, dat.plot,
                   c("Raw data values",
                     "log2(Raw data values)",
                     "vsn(Raw data values)"))
  plot <- gridExtra::marrangeGrob(all.plots, nrow = 1, ncol = 1, top = NULL)
  if (!is.null(path))
    ggsave(filename = path, plot = plot, width = width, height = height)
  return(all.plots)
}

#' ggplot boxplot applied to each data source
#' @noRd
ms_gg_boxplot <- function(x, title) {
  p <- ggplot(x, aes_(x = quote(Sample), y = quote(Expression))) +
    stat_boxplot(geom = "errorbar", width = 0.4) +
    geom_boxplot() +
    theme_linedraw() + 
    theme(plot.title = element_text(face = "bold"),
          panel.grid = element_blank(),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
    ggtitle(title)
  return(p)
}

#' @inheritParams ms_plot
#' @param g vector of treatment groups
#' @param title vector of titles for each \code{g}
#' @name ms_plot
#' @export
ms_mean_var <- function(x, g, title, width = 8, height = 10, path = NULL) {
  dat.plot <- tidyr::gather(as.data.frame(x[["vsn"]]),
                            key = "Sample", value = "Expression")
  p1 <- ms_gg_boxplot(dat.plot, "vsn(Raw data values)")
  p23 <- Map(function(g, t)
    vsn::meanSdPlot(x$vsn[, grep(g, colnames(x$vsn))], plot = FALSE)$gg +
      ggtitle(paste("vsn", t)), g = g, t = title)
  all.plots <- list(p1, p23[[1]], p23[[2]])
  plot <- gridExtra::marrangeGrob(all.plots, nrow = 1, ncol = 1, top = NULL)
  if (!is.null(path))
    ggsave(filename = path, plot = plot, width = width, height = height)
  return(all.plots)
}