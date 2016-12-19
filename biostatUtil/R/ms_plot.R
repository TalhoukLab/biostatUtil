#' Boxplot of different MS expression data values
#' 
#' Generate boxplot for raw, log2, and vsn data values
#' 
#' @param x data object returned by \code{ms_process}
#' @param path path to save boxplot to. Device is pdf.
#' @param width width of plot
#' @param height height of plot
#' @return A pdf with three boxplots is saved to disk location specified by
#'   \code{path}: the raw data values and the log2 and vsn transformed values.
#' @name ms_plot
#' @family Mass Spectrometry
#' @author Derek Chiu
#' @export
ms_boxplot <- function(x, path, width = 8, height = 10) {
  dat.plot <- lapply(x[c("raw", "l2", "vsn")], function(y)
    gather(as.data.frame(y), key = Sample, value = Expression))
  all.plots <- Map(ms_gg_boxplot, dat.plot,
                   c("Raw data values",
                     "log2(Raw data values)",
                     "vsn(Raw data values)"))
  plot <- gridExtra::marrangeGrob(all.plots, nrow = 1, ncol = 1)
  ggsave(filename = path, plot = plot, width = width, height = height)
}

#' ggplot boxplot applied to each data source
#' @noRd
ms_gg_boxplot <- function(x, title) {
  p <- ggplot(x, aes(x = Sample, y = Expression)) +
    stat_boxplot(geom = "errorbar", width = 0.4) +
    geom_boxplot() +
    theme_linedraw() + 
    theme(panel.grid = element_blank(),
          plot.title = element_text(face = "bold")) +
    ggtitle(title)
  return(p)
}

#' Mean-Variance Relationship
#' 
#' Generate mean-sd plots for the VSN data values.
#' 
#' @inheritParams ms_plot
#' @param g vector of treatment groups
#' @param title vector of titles for each treatment group
#' @return A pdf is saved to disk location specified by \code{path}: the vsn
#'   transformed values and mean-sd plots for each treatment group.
#' @name ms_plot
#' @family Mass Spectrometry
#' @author Derek Chiu
#' @export
ms_mean_var <- function(x, g, title, path, width = 8, height = 10, las = 0) {
  pdf(file = path, width = width, height = height, useDingbats = FALSE)
  par(las = las)
  boxplot(x$vsn, main = "vsn(Raw data values)")
  capture.output(Map(function(g, t)
    vsn::meanSdPlot(x$vsn[, grep(g, colnames(x$vsn))], plot = FALSE)$gg +
      ggtitle(paste("vsn", t)), g = g, t = title))
  dev.off()
  par(las = 0)
}