#' Do a jitterplot among subtypes
#' 
#' Generates stripchart (jitter plot) of a biomarker split on a categorical subtype.
#' 
#' Expects subtype variable to be a factor. Also, biomarker and subtype variables
#' have missing cases as \code{NA}.
#' 
#' @param input.d input \code{data.frame}
#' @param data.description title description
#' @param biomarker.var.name variable name of biomarker to plot
#' @param biomarker.name x-axis label for biomarker name
#' @param subtype.var.name variable name of subtype to separate biomarker
#' @param subtype.name y-axis label for subtype name
#' @param pch jitterplot dot type. For example, 20 = small dot, 16 = big dot.
#' @param jitter scalar indicating amount of jitter (spread of points)
#' @param digits number of digits to round to
#' @param cex.axis scalar indicating amount of scaling for axes
#' @return A jitterplot shown across different subtypes
#' @author Samuel Leung
#' @export
doJitterplotAmongSubtypes <- function(input.d, data.description,
                                      biomarker.var.name, biomarker.name,
                                      subtype.var.name, subtype.name,
                                      pch = ".", jitter = 0.05, digits = 3,
                                      cex.axis = 0.9) {
  
  temp.d <- input.d[(!is.na(input.d[, biomarker.var.name])) &
                      (!is.na(input.d[, subtype.var.name])), ]
  biomarker <- temp.d[, biomarker.var.name]
  subtype <- temp.d[, subtype.var.name]
  xbar <- tapply(biomarker, subtype, bootMean)
  test.name <- "Kruskal-Wallis"
  p.value <- kruskal.test(biomarker ~ subtype)$p.value
  if (length(names(table(subtype))) == 2) {
    test.name <- "Wilcoxon Rank Sum"
    p.value <- wilcox.test(biomarker ~ subtype)$p.value
  }
  par(mar = c(5.1, 4.1, 5.1, 2.1))
  stripchart(biomarker ~ subtype, method = "jitter", jitter = jitter,
             pch = pch, cex.axis = cex.axis, vert = TRUE,
             group.names = paste0(
               paste0(names(xbar), rep("\nn=", length(xbar))),
               sapply(xbar, function(x) x$n, USE.NAMES = FALSE)),
             ylab = biomarker.name, xlab = subtype.name,
             main = paste0(data.description, "\n", test.name, " test P=",
                           format(p.value, digits = digits)))
  arrows(1:length(xbar), sapply(xbar, function(x) x$ci[1], USE.NAMES = FALSE),
         1:length(xbar), sapply(xbar, function(x) x$ci[2], USE.NAMES = FALSE),
         angle = 90, code = 3, length = 0.1)
  points(sapply(xbar, function(x) x$obs.mean, USE.NAMES = FALSE),
         pch = 4, type = "p", cex = 2)
}