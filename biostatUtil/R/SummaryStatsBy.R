#' Generate cohort characteristics
#' @param data The \code{data.frame} containing the data
#' @param by1 split variable 1
#' @param by2 split variable 2
#' @param var.names The variables that you want the statistics for
#' @param stats statistics to show compute for continuous variables
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
                           stats = c("mean", "sd", "median", "IQR", "range",
                                     "missing"),
                           marker.description = by1, groups.description = by2,
                           var.descriptions = var.names, digits = 3,
                           missing.codes.highlight = NULL,
                           missing.codes = c("N/A", "", "Unk"),
                           decimal = 0, caption = NA) {
  . <- NULL
  var.dat <- data[, var.names]
  facets <- data[, c(by1, by2)]
  types <- sapply(var.dat, class)
  if (length(var.names) < 2) {
    if (all(types %in% c("numeric", "integer"))) {
      num.var <- var.names
    } else {
      fac.var <- var.names
    }
  } else {
    num.var <- names(types)[which(types %in% c("numeric", "integer"))]
    fac.var <- names(types)[which(types %in% c("factor", "character"))]
  }
  if (length(num.var) < 2) {
    num.dat <- fac.dat <- cbind(var.dat, facets) %>% 
      set_colnames(c(var.names, by1, by2))
  } else {
    num.dat <- cbind(var.dat[, num.var], facets) %>% 
      set_colnames(c(num.var, by1, by2))
    fac.dat <- cbind(var.dat[, fac.var], facets) %>% 
      set_colnames(c(fac.var, by1, by2))
  }
  if ("range" %in% stats) {
    rows <- gsub("range", "min", stats) %>% 
      append("max", match("min", .))
  } else {
    rows <- stats
  }
  row.order <- unlist(lapply(num.var, function(x)
    paste0(x, c("", paste0(".", rows)))))
  if (length(num.var) != 0) {
    f <- as.formula(paste(paste(num.var, collapse = " + "), "~",
                          paste(by1, by2, sep = " + ")))
    values <- doBy::summaryBy(f, num.dat, FUN = contSumFunc,
                              digits = digits, stats = stats)
    facs <- values[, c(by1, by2)]
    if (length(num.var) < 2) {
      main <- values %>% 
        extract(, -match(c(by1, by2), colnames(.))) %>% 
        mutate(filler = "") %>% 
        set_colnames(gsub("filler", eval(num.var), colnames(.))) %>% 
        t %>% 
        extract(row.order, ) %>% 
        set_rownames(unlist(lapply(paste0("**", num.var, "**"),
                                   function(x) c(x, rows)))) %>% 
        set_colnames(apply(mapply(function(x, y) paste(x, y, sep = "="),
                                  names(facs), facs), 1, function(x)
                                    paste(x, collapse = ", ")))
      if (all(c("mean", "sd") %in% stats)) {
        main <- main %>% 
          rbind(mean = paste(.[which(rownames(.) == "mean"), ],
                             .[which(rownames(.) == "sd"), ],
                             sep = " &#177; ")) %>% 
          extract(-match("mean", rownames(.)), ) %>% 
          extract(c(1, match(rows, rownames(.))), ) %>% 
          extract(-match("sd", rownames(.)), )
      }
      if ("range" %in% stats) {
        main <- main %>% 
          rbind(range = paste(.[which(rownames(.) == "min"), ],
                              .[which(rownames(.) == "max"), ],
                              sep = "-")) %>% 
          extract(-match(c("min", "max"), rownames(.)), ) %>% 
          extract(c(1, match(stats, rownames(.))), )  %>% 
          extract(-match(NA, rownames(.)), )
      }
    } else {
      if (all(c("mean", "sd") %in% stats)) {
        means <- values[, grep("\\.mean", names(values))]
        sds <- values[, grep("\\.sd", names(values))]
        ms <- apply(mapply(function(x, y) paste(x, y, sep = " &#177; "),
                           means, sds), 2, cbind)
        rows <- rows[-grep("sd", rows)]
        row.order <- row.order[-grep("\\.sd", row.order)]
      } else if ("mean" %in% stats & !"sd" %in% stats) {
        ms <- values[, grep("\\.mean", names(values))]
      } else if ("sd" %in% stats & !"mean" %in% stats) {
        ms <- values[, grep("\\.sd", names(values))]
      } else {
        ms <- data.frame(row.names = 1:nrow(facs))
      }
      if ("median" %in% stats) {
        med <- values[, grep("\\.median", names(values))]
      } else {
        med <- data.frame(row.names = 1:nrow(facs))
      }
      if ("IQR" %in% stats) {
        iqr <- values[, grep("\\.IQR", names(values))]
      } else {
        iqr <- data.frame(row.names = 1:nrow(facs))
      }
      if ("range" %in% stats) {
        min <- values[, grep("\\.min", names(values))]
        max <- values[, grep("\\.max", names(values))]
        range <- mapply(function(x, y) paste(x, y, sep = "-"), min, max) %>%
          set_colnames(gsub("min", "range", colnames(.)))
        rows <- rows %>% 
          stringr::str_replace_all("min", "range") %>% 
          extract(-grep("max", .))
        row.order <- row.order %>% 
          stringr::str_replace_all("min", "range") %>% 
          extract(-grep("max", .))
      } else {
        range <- data.frame(row.names = 1:nrow(facs))
      }
      if ("missing" %in% stats) {
        miss <- values[, grep("\\.missing", names(values), value = T)]
      } else {
        miss <- data.frame(row.names = 1:nrow(facs))
      }
      main <- cbind(ms, med, iqr, range, miss,
                    matrix(rep("", sum(table(facets[, c(by1, by2)]) > 0)),
                           ncol = length(var.names),
                           dimnames = list(NULL, var.names))) %>% 
        t %>% 
        extract(row.order, ) %>% 
        set_rownames(unlist(lapply(paste0("**", var.names, "**"),
                                   function(x) c(x, rows)))) %>% 
        set_colnames(apply(mapply(function(x, y) paste(x, y, sep = "="),
                                  names(facs), facs), 1, function(x)
                                    paste(x, collapse = ", ")))
    }
  } else {
    main <- NULL
  }
  if (length(fac.var) != 0) {
    g <- as.formula(paste(paste(fac.var, collapse = " + "), "~",
                          paste(by1, by2, sep = " + ")))
    fac.values <- as.data.frame(as.matrix(aggregate(g, fac.dat, summary)))
    if (length(fac.var) < 2) {
      main2 <- fac.values %>% 
        extract(order(.[, by1]), -match(c(by1, by2), colnames(.))) %>% 
        t %>% 
        rbind("", .) %>% 
        set_rownames(c(paste0("**", fac.var, "**"),
                       levels(fac.dat[, fac.var]))) %>% 
        set_colnames(apply(mapply(function(x, y) paste(x, y, sep = "="),
                                  names(facs), facs), 1, function(x)
                                    paste(x, collapse = ", ")))
    }
  } else {
    main2 <- NULL
  }
  return(pander::pandoc.table(rbind(main, main2), split.table = Inf,
                              emphasize.rownames = F))
}
# SummaryStatsBy(data = mtcars, by1 = "cyl", by2 = "gear", var.names = c("qsec", "vs"))
# SummaryStatsBy2(data = mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg"))
# a <- mtcars[, c("vs", "cyl", "gear")]
# b <- aggregate(vs ~ cyl + gear, a, summary)
# f1 <- as.formula(paste(paste(var.names, collapse = " + "), "~", by1))
# f2 <- as.formula(paste(paste(var.names, collapse = " + "), "~", by2))
# tot1 <- summaryBy(f1, t.dat, FUN = contSumFunc)
# tot2 <- summaryBy(f2, t.dat, FUN = contSumFunc)
# tot <- c("Total", contSumFunc(t.dat[, var.names]))