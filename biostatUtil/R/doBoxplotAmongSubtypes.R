#' Do a boxplot among subtypes
#' 
#' A stripchart (jitter plot) of a biomarker and some categorical subtype
#' is superimposed on top of the boxplot
#' @param input.d input data.frame
#' @param data.description boxplot title description
#' @param biomarker.var.name biomarker variable name in \code{input.d} to graph on
#' @param biomarker.name boxplot y-axis label
#' @param subtype.var.name subtype variable name in \code{input.d} to graph on
#' @param subtype.name boxplot x-axis label
#' @param pch stripchart plot style
#' @param jitter amount of jitter in stripchart
#' @param digits number of digits to round to
#' @param ... additional arguments to \code{boxplot}
#' @return a boxplot of biomarker values across different subtypes
#' @note Expects subtype variable to be a factor. Also expects biomarker
#' and subtype variable missing values to be \code{NA}
#' @author Samuel Leung
#' @export
doBoxplotAmongSubtypes <- function(input.d, data.description,
                                   biomarker.var.name, biomarker.name,
                                   subtype.var.name, subtype.name, pch = 4,
                                   jitter = 0.1, digits = 2, ...) {
  temp.d <- input.d[(!is.na(input.d[, biomarker.var.name])) & 
                      (!is.na(input.d[, subtype.var.name])), ]
  biomarker <- temp.d[, biomarker.var.name]
  if (is.factor(temp.d[, subtype.var.name])) {
    subtype <- droplevels(temp.d[, subtype.var.name])
  } else {
    subtype <- temp.d[, subtype.var.name]
  }
  xbar <- tapply(biomarker, subtype, bootMean)
  test.name <- "Kruskal-Wallis"
  p.value <- kruskal.test(biomarker ~ subtype)$p.value
  if (length(names(table(subtype))) == 2) {
    test.name <- "Wilcoxon Rank Sum"
    p.value <- wilcox.test(biomarker ~ subtype)$p.value
  }
  
  boxplot(biomarker ~ subtype,
          names = paste0(
            paste0(names(xbar), rep("\nn=",length(xbar))),
            sapply(xbar, function(x) (x$n), USE.NAMES = FALSE)),
          ylab = biomarker.name, xlab = subtype.name,
          main = paste0(data.description, "\n", test.name, " test P=",
                        format(p.value, digits = digits)),
          outline = FALSE, ...)
  stripchart(jitter(biomarker) ~ subtype, vertical = T, method = "jitter",
             pch = pch, add = T, jitter = jitter)
} 