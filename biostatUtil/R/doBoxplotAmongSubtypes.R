#' Do a boxplot among subtypes
#' @export
####################################################################
# function to generate stripchart (jitter plot) of 
# a biomarker x some categoriical subtype on top of a boxplot
#
# NOTE:
# - expect subtype variable be factor
# - expect biomarker and subtype variable missing to be NA
#
# example of pch ... 20=small dot, 16=bigger dot
#
doBoxplotAmongSubtypes <- function(input.d, data.description,
                                   biomarker.var.name, biomarker.name,
                                   subtype.var.name, subtype.name, pch = 4,
                                   jitter = 0.1, digits = 2, boot.mean, ...) {
  temp.d <- input.d[(!is.na(input.d[, biomarker.var.name])) & 
                      (!is.na(input.d[, subtype.var.name])), ]
  biomarker <- temp.d[, biomarker.var.name]
  if (is.factor(temp.d[, subtype.var.name])) {
    subtype <- droplevels(temp.d[, subtype.var.name])
  } else {
    subtype <- temp.d[, subtype.var.name]
  }
  xbar <- tapply(biomarker, subtype, boot.mean)
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