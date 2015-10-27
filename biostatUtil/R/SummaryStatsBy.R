#' Generate cohort characteristics
#' @param input.d The \code{data.frame} containing the data
#' @param marker.name The variable that you want to split into different columns
#' @param marker.description The description for the variable(s) to split
#' @param var.names The variables that you want the statistics for
#' @author Aline Talhouk, Derek Chiu
#' @import doBy
#' @export
#' 
SummaryStatsby <- function(input.d, by1, by2, var.names,
                           marker.description = by1, groups.description = by2,
                           var.descriptions = var.names, digits = 1,
                           missing.codes.highlight = NULL,
                           missing.codes = c("N/A", "", "Unk"),
                           decimal = 0, caption = NA) {
  t.dat <- input.d[, c(var.names, by1, by2)]
  var.dat <- t.dat[, var.names]
  types <- ifelse(is.null(dim(var.dat)), class(var.dat), apply(var.dat, 2, class))
  row.order <- unlist(lapply(var.names, function(x)
    paste0(x, c("", ".mean", ".median", ".IQR", ".range1", ".missing"))))
  contSumFunc <- function(x) {
    round(c(mean = mean(x, na.rm = TRUE), s = sd(x, na.rm = TRUE), 
            median = median(x, na.rm = TRUE), IQR = IQR(x, na.rm = TRUE),
            range = range(x, na.rm = TRUE), missing = sum(is.na(x))), digits)
  }
  if (all(types == "numeric")) {
    f <- as.formula(paste(paste(var.names, collapse = " + "), "~", paste(by1, by2, sep = " + ")))
    values <- summaryBy(f, t.dat, FUN = contSumFunc)
    means <- values[, grep("\\.mean", names(values), value = T)]
    sds <- values[, grep("\\.s", names(values), value = T)]
    min <- values[, grep("\\.range1", names(values), value = T)]
    max <- values[, grep("\\.range2", names(values), value = T)]
    # totalvarby1 <- summaryBy(var ~ varby1, data = t.dat, FUN = contSumFunc)
    # totalvarby2 <- summaryBy(var ~ varby2, data = t.dat, FUN = contSumFunc)
    # total <- c("Total", contSumFunc(t.dat[, "var"]))
    main <- t(cbind(
      # c(sapply(seq_len(length(levels(varby1))), function(i)
      # c(levels(varby1)[i], rep(" ", each = length(levels(varby2)) - 1)))),
      # rep(levels(varby2), length(levels(varby1))),
      # mean = paste(values[, "var.mean"], values[, "var.s"], sep = " &#177; "),
      apply(mapply(function(x, y) paste(x, y, sep = " &#177; "), means, sds), 2, cbind),
      values[, grep("\\.median", names(values), value = T)],
      values[, grep("\\.IQR", names(values), value = T)],
      apply(mapply(function(x, y) paste(x, y, sep = "-"), min, max), 2, cbind),
      values[, grep("\\.missing", names(values), value = T)],
      matrix(rep("", length(row.order) - 2), ncol = length(var.names),
             dimnames = list(NULL, var.names))))
    main <- main[row.order, ]
    return(main)
    # colnames(main) <- c(" ", " ", "mean", "median", "range", "missing")
  } else {
    return(NA)
  }
  # pander::pandoc.table.return(main, justify = 'left', style = "multiline")
}

SummaryStatsby(input.d = mtcars, by1 = "cyl", by2 = "vs", var.names = c("mpg", "qsec"))
