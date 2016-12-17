#' Boxplot of different MS expression data values
#' 
#' Generate boxplot for raw, log2, and vsn data values
#' 
#' @param x data object returned by \code{ms_process}
#' @param path path to save boxplot to. Device is pdf.
#' @param width width of plot
#' @param height height of plot
#' @param las angle of axis labels
#' @return A pdf with three boxplots is saved to disk location specified by
#'   \code{path}: the raw data values and the log2 and vsn transformed values.
#' @author Derek Chiu
#' @export
ms_boxplot <- function(x, path, width = 8, height = 10, las = 0) {
  title <- "Raw data values"
  pdf(file = path, width = width, height = height, useDingbats = FALSE)
  par(las = las)
  Map(boxplot, x = x[-1], main = c(title, paste0("log2(", title, ")"),
                                   paste0("vsn(", title, ")")))
  dev.off()
  par(las = 0)
}

#' Mean-Variance Relationship
#' 
#' Generate mean-sd plots for the VSN data values.
#' 
#' @inheritParams ms_boxplot
#' @param g vector of treatment groups
#' @param title vector of titles for each treatment group
#' @return A pdf is saved to disk location specified by \code{path}: the vsn
#'   transformed values and mean-sd plots for each treatment group.
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