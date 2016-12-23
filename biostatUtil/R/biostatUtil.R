#' biostatUtil: utility functions for biostatistical projects
#' 
#' biostatUtil provides a wide variety of data analysis, visualization, and 
#' presentation functions used in biostatistical projects.
#' 
#' This package is currently in development stage while still lacking 
#' comprehensive documentation and testing. The goal is to ultimately separate 
#' biostatUtil into smaller, more cohesive packages each with better defined 
#' purposes.
#' 
#' Functions can be categorized into one of the following: \itemize{
#' \item Survival analysis
#' \item Character manipulation
#' \item Date manipulation
#' \item Plotting survival-related figures
#' \item Miscellaneous functions
#' }
#' 
#' @docType package
#' @name biostatUtil
#' @import dplyr htmltools ggplot2 survival
#' @importFrom coin lbl_test statistic pvalue
#' @importFrom Deducer likelihood.test
#' @importFrom graphics arrows barplot box boxplot hist legend lines mtext par
#'   plot points stripchart title text
#' @importFrom grDevices cairo_pdf dev.off pdf png tiff
#' @importFrom KMsurv lifetab
#' @importFrom magrittr extract set_colnames set_rownames set_names use_series
#' @importFrom stats addmargins aggregate AIC anova as.formula chisq.test
#'   confint cor cor.test dnorm extractAIC fisher.test IQR kruskal.test logLik
#'   median p.adjust qnorm quantile relevel sd setNames wilcox.test
#'   var
#' @importFrom utils as.roman capture.output combn read.table tail
NULL