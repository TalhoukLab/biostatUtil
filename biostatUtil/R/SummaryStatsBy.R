#' Generate cohort characteristics
#' @param data The \code{data.frame} containing the data
#' @param by1 split variable 1
#' @param by2 split variable 2
#' @param var.names The variables that you want the statistics for
#' @param marker.description The description for the variable(s) to split
#' @param groups.description The groupings of the marker
#' @param var.descriptions descriptions of \code{var.names}
#' @param digits number of digits to round to
#' @param missing.codes.highlight NAs
#' @param missing.codes search strings to detect missing entries
#' @param decimal number of decimal places
#' @param caption caption for summary table
#' @author Aline Talhouk, Derek Chiu
#' @import doBy dplyr
#' @importFrom magrittr set_colnames set_rownames extract
#' @export
SummaryStatsBy <- function(data, by1, by2, var.names,
                           marker.description = by1, groups.description = by2,
                           var.descriptions = var.names, digits = 3,
                           missing.codes.highlight = NULL,
                           missing.codes = c("N/A", "", "Unk"),
                           decimal = 0, caption = NA) {
  var.dat <- data[, var.names]
  facets <- data[, c(by1, by2)]
  if (is.null(dim(var.dat)))
    types <- class(var.dat)
  else
    types <- sapply(var.dat, class)
  num.dat <- cbind(var.dat[, which(types == "numeric")], facets)
  fac.dat <- cbind(var.dat[, which(types == "factor")], facets)
  rows <- c("mean", "median", "IQR", "range", "missing")
  row.order <- unlist(lapply(var.names, function(x)
    paste0(x, c("", ".mean", ".median", ".IQR", ".range1", ".missing"))))
    f <- as.formula(paste(paste(var.names, collapse = " + "), "~",
                          paste(by1, by2, sep = " + ")))
    values <- summaryBy(f, num.dat, FUN = contSumFunc, digits = digits)
    facs <- values[, c(by1, by2)]
    means <- values[, grep("\\.mean", names(values), value = T)]
    sds <- values[, grep("\\.s", names(values), value = T)]
    min <- values[, grep("\\.range1", names(values), value = T)]
    max <- values[, grep("\\.range2", names(values), value = T)]
    f1 <- as.formula(paste(paste(var.names, collapse = " + "), "~", by1))
    f2 <- as.formula(paste(paste(var.names, collapse = " + "), "~", by2))
    # tot1 <- summaryBy(f1, t.dat, FUN = contSumFunc)
    # tot2 <- summaryBy(f2, t.dat, FUN = contSumFunc)
    # tot <- c("Total", contSumFunc(t.dat[, var.names]))
    main <- cbind(
      # c(sapply(seq_len(length(levels(varby1))), function(i)
      # c(levels(varby1)[i], rep(" ", each = length(levels(varby2)) - 1)))),
      # rep(levels(varby2), length(levels(varby1))),
      # mean = paste(values[, "var.mean"], values[, "var.s"], sep = " &#177; "),
      apply(mapply(function(x, y) paste(x, y, sep = " &#177; "), means, sds),
            2, cbind),
      values[, grep("\\.median", names(values), value = T)],
      values[, grep("\\.IQR", names(values), value = T)],
      apply(mapply(function(x, y) paste(x, y, sep = "-"), min, max), 2, cbind),
      values[, grep("\\.missing", names(values), value = T)],
      matrix(rep("", sum(table(facets[, c(by1, by2)]) > 0)),
             ncol = length(var.names),
             dimnames = list(NULL, var.names))) %>% 
      t %>% 
      extract(row.order, ) %>% 
      set_rownames(unlist(lapply(var.names, function(x) c(x, rows)))) %>% 
      set_colnames(apply(mapply(function(x, y) paste(x, y, sep = "="),
                                x = names(facs), y = facs), 1, function(x)
                                  paste(x, collapse = ", ")))
    rownames(main)[c(1, 7)] <- paste0("**", rownames(main)[c(1, 7)], "**")
  pander::pandoc.table(main, split.table = Inf, emphasize.rownames = F)
}

# SummaryStatsBy(data = mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg", "qsec"))
