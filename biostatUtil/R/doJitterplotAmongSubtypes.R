#' Do a jitterplot among subtypes
#' @export
####################################################################
# function to generate stripchart (jitter plot) of 
# a biomarker x some categoriical subtype
# i.e. similar to boxplot
#
# NOTE:
# - expect subtype variable be factor
# - expect biomarker and subtype variable missing to be NA
#
# example of pch ... 20=small dot, 16=bigger dot
#
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