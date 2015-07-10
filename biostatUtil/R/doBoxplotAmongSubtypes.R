#' Do a boxplot among subtypes
#' @export
doBoxplotAmongSubtypes <- function(input.d, data.description,
                                   biomarker.var.name, biomarker.name,
                                   subtype.var.name, subtype.name, pch = 4,
                                   jitter = 0.1, digits = 2, boot.mean, ...) {
  temp.d <- input.d[
    (!is.na(input.d[, biomarker.var.name])) & 
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
  
  boxplot(
    biomarker ~ subtype,
    names = paste0(
      paste0(
        names(xbar),
        rep("\nn=",length(xbar))),
      sapply(xbar, function(x) {return(x$n)}, USE.NAMES = FALSE)),
    ylab = biomarker.name,
    xlab = subtype.name,
    main = paste0(
      data.description,
      "\n", test.name, " test P=", format(p.value, digits = digits)),
    outline = FALSE, # no outliers, they will be drawn by stripchart
    ...
  )
  stripchart(jitter(biomarker) ~ subtype, vertical = T, method = "jitter",
             pch = pch, add = T, jitter = jitter)
} 