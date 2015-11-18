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
#' @param format format to return the table in. Either "pandoc" (for Word and PDF),
#' "html", or "long" format for graphing and data manipulation using raw values.
#' @author Aline Talhouk, Derek Chiu
#' @import doBy dplyr
#' @importFrom magrittr set_colnames set_rownames extract use_series
#' @export
#' @examples 
#' mtcars$vs <- as.factor(mtcars$vs); mtcars$am <- as.factor(mtcars$am)
#' SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg", "vs", "qsec", "am"))
#' SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("vs", "qsec"))
SummaryStatsBy <- function(data, by1, by2, var.names,
                           stats = c("mean", "sd", "median", "IQR", "range",
                                     "missing"),
                           marker.description = by1, groups.description = by2,
                           var.descriptions = var.names, digits = 3,
                           format = c("pandoc", "html", "long")) {
  . <- stat <- value <- grp <- vars <- lev <- NULL
  var.dat <- data[, var.names]
  facets <- data[, c(by1, by2)]
  types <- sapply(var.dat, class)
  num.ind <- types %in% c("numeric", "integer")
  fac.ind <- types %in% c("factor", "character")
  # Separate numeric and factor components
  if (length(var.names) < 2) {  # Single response
    if (all(num.ind)) {  # Numeric case
      num.var <- var.names
      num.dat <- cbind(var.dat, facets) %>% 
        set_colnames(c(num.var, by1, by2))
      fac.var <- fac.dat <- NULL
    } else if (all(fac.ind)) {  # Factor case
      fac.var <- var.names
      fac.dat <- cbind(var.dat, facets) %>% 
        set_colnames(c(fac.var, by1, by2))
      num.var <- num.dat <- NULL
    } else {
      stop('Variable must be numeric, integer, factor, or character.')
    }
  } else {  # Multiple responses
    if (length(which(num.ind)) > 0) {  # Numeric cases
      num.var <- names(types)[which(num.ind)]
      num.dat <- cbind(var.dat[, num.var], facets) %>% 
        set_colnames(c(num.var, by1, by2))
    } else {
      num.var <- num.dat <- NULL
    }
    if (length(which(fac.ind)) > 0) {  # Factor cases
      fac.var <- names(types)[which(fac.ind)]
      fac.dat <- cbind(var.dat[, fac.var], facets) %>% 
        set_colnames(c(fac.var, by1, by2))
    } else {
      fac.var <- fac.dat <- NULL
    }
  }
  if ("range" %in% stats) {  # Replace "range" input with "min" & "max"
    rows <- gsub("range", "min", stats) %>%
      append("max", match("min", .))
  } else {
    rows <- stats
  }
  if (length(num.var) > 0) {  # Compute numerical summaries
    row.order <- unlist(lapply(num.var, function(x)
      paste0(x, c("", paste0(".", rows)))))
    num.form <- as.formula(paste(paste(num.var, collapse = " + "), "~",
                                 paste(by1, by2, sep = " + ")))
    num.values <- doBy::summaryBy(num.form, num.dat, FUN = contSumFunc,
                                  digits = digits, stats = stats)
    num.values.tot <- num.var %>% 
      paste(collapse = " + ") %>% 
      paste("~", by1) %>% 
      as.formula() %>% 
      doBy::summaryBy(., num.dat, FUN = contSumFunc, digits = digits) %>% 
      cbind(matrix(rep(NA, nrow(.)), dimnames = list(NULL, by2))) %>% 
      extract(, c(1, ncol(.), 2:(ncol(.) - 1))) %>% 
      rbind(num.values, .) %>% 
      extract(order(.[, by1]), ) %>% 
      set_rownames(NULL)
    num.values.long <- num.values %>% 
      tidyr::gather(stat, value, -match(c(by1, by2), colnames(.))) %>%
      tidyr::separate(stat, c("var", "stat"), "\\.") %>%
      extract(order(.[, "var"], .[, by1], .[, by2]), ) %>% 
      set_rownames(NULL)
    facs <- num.values.tot[, c(by1, by2)]
    if (length(num.var) < 2) {  # Single numeric response
      num.result <- num.values.tot %>% 
        extract(, -match(c(by1, by2), colnames(.))) %>% 
        mutate(filler = "") %>% 
        set_colnames(gsub("filler", eval(num.var), colnames(.))) %>%
        t() %>% 
        extract(row.order, ) %>% 
        set_rownames(unlist(lapply(paste0("**", num.var, "**"),
                                   function(x) c(x, rows)))) %>% 
        set_colnames(apply(mapply(function(x, y) paste(x, y, sep = "="),
                                  names(facs), facs), 1, function(x)
                                    paste(x, collapse = ", "))) %>% 
        set_colnames(ifelse(grepl("NA", colnames(.)),
                            stringr::str_split_fixed(
                              colnames(.), ", ",2)[, 1], colnames(.)))
      if (all(c("mean", "sd") %in% stats)) {
        num.result <- num.result %>% 
          rbind(mean = paste(.[which(rownames(.) == "mean"), ],
                             .[which(rownames(.) == "sd"), ],
                             sep = " &#177; ")) %>% 
          extract(-match("mean", rownames(.)), ) %>% 
          extract(c(1, match(rows, rownames(.))), ) %>% 
          extract(-match("sd", rownames(.)), )
      }
      if ("range" %in% stats) {
        num.result <- num.result %>% 
          rbind(range = paste(.[which(rownames(.) == "min"), ],
                              .[which(rownames(.) == "max"), ],
                              sep = "-")) %>% 
          extract(-match(c("min", "max"), rownames(.)), ) %>% 
          extract(c(1, match(stats, rownames(.))), )  %>% 
          extract(-match(NA, rownames(.)), )
      }
    } else {  # Multiple numeric response
      if (all(c("mean", "sd") %in% stats)) {
        means <- num.values.tot[, grep("\\.mean", names(num.values.tot))]
        sds <- num.values.tot[, grep("\\.sd", names(num.values.tot))]
        ms <- apply(mapply(function(x, y) paste(x, y, sep = " &#177; "),
                           means, sds), 2, cbind)
        rows <- rows[-grep("sd", rows)]
        row.order <- row.order[-grep("\\.sd", row.order)]
      } else if ("mean" %in% stats & !"sd" %in% stats) {
        ms <- num.values.tot[, grep("\\.mean", names(num.values.tot))]
      } else if ("sd" %in% stats & !"mean" %in% stats) {
        ms <- num.values.tot[, grep("\\.sd", names(num.values.tot))]
      } else {
        ms <- data.frame(row.names = 1:nrow(facs))
      }
      if ("median" %in% stats) {
        med <- num.values.tot[, grep("\\.median", names(num.values.tot))]
      } else {
        med <- data.frame(row.names = 1:nrow(facs))
      }
      if ("IQR" %in% stats) {
        iqr <- num.values.tot[, grep("\\.IQR", names(num.values.tot))]
      } else {
        iqr <- data.frame(row.names = 1:nrow(facs))
      }
      if ("range" %in% stats) {
        min <- num.values.tot[, grep("\\.min", names(num.values.tot))]
        max <- num.values.tot[, grep("\\.max", names(num.values.tot))]
        range <- mapply(function(x, y) paste(x, y, sep = "-"), min, max) %>%
          set_colnames(gsub("min", "range", colnames(.)))
        rows <- rows %>% 
          gsub("min", "range", .) %>% 
          extract(-grep("max", .))
        row.order <- row.order %>% 
          stringr::str_replace_all("min", "range") %>% 
          extract(-grep("max", .))
      } else {
        range <- data.frame(row.names = 1:nrow(facs))
      }
      if ("missing" %in% stats) {
        miss <- num.values.tot[, grep("\\.missing", names(num.values.tot),
                                      value = T)]
      } else {
        miss <- data.frame(row.names = 1:nrow(facs))
      }
      num.result <- cbind(ms, med, iqr, range, miss,
                          matrix(rep("", nrow(facs) * length(num.var)),
                                 ncol = length(num.var),
                                 dimnames = list(NULL, num.var))) %>% 
        t() %>% 
        extract(row.order, ) %>% 
        set_rownames(unlist(lapply(paste0("**", num.var, "**"),
                                   function(x) c(x, rows)))) %>% 
        set_colnames(mapply(function(x, y) paste(x, y, sep = "="),
                                  names(facs), facs[order(facs[, by1]), ]) %>%
                       apply(., 1, function(x) paste(x, collapse = ", ")) %>% 
                       ifelse(grepl("NA", .), 
                              stringr::str_split_fixed(., ", ", 2)[, 1], .))
    }
  } else {
    num.result <- num.values.long <- NULL
  }
  if (length(fac.var) > 0) {  # Compute factor summaries
    if (length(fac.var) > 1) {
      row.order <- fac.var %>% 
        mapply(function(x, y) paste0(x, ".", levels(y)), ., fac.dat[, .]) %>%
        rbind(colnames(.), .) %>% 
        as.vector()
    } else {
      row.order <- c(fac.var, paste0(fac.var, ".", levels(fac.dat[, fac.var])))
    }
    fac.values <- fac.var %>% 
      sapply(function(x) as.formula(paste(x, "~",
                                          paste(by1, by2, sep = " + ")))) %>%
      lapply(aggregate, fac.dat, summary) %>% 
      Reduce(merge, .) %>% 
      extract(order(.[, by1]), ) %>% 
      as.matrix()
    fac.values.tot <- fac.var %>% 
      sapply(function(x) as.formula(paste(x, "~",
                                          paste(by1, sep = " + ")))) %>%
      lapply(aggregate, fac.dat, summary) %>% 
      Reduce(merge, .) %>% 
      extract(order(.[, by1]), ) %>% 
      as.matrix() %>% 
      cbind(matrix(rep(NA, nrow(.)), dimnames = list(NULL, by2))) %>% 
      extract(, c(1, ncol(.), 2:(ncol(.) - 1)))
    fac.values.res <- fac.values.tot %>% 
      as.data.frame() %>% 
      split(.[, by1]) %>% 
      lapply(function(x) x %>% 
               extract(, -match(c(by1, by2), colnames(.))) %>% 
               t() %>% 
               as.data.frame() %>% 
               mutate(grp = factor(stringr::str_split_fixed(
                 rownames(.), "\\.", 2)[, 1], levels = fac.var)) %>% 
               group_by(grp) %>% 
               do(vars = colPercent(.[, -ncol(.)]))) %>% 
      do.call(rbind, .) %>% 
      split(.$grp) %>% 
      lapply(function(x) do.call(cbind, rbind(x$vars)) %>% 
               cbind(., lev = rep(letters[1:(nrow(.) / 2)], 2)) %>% 
               as.data.frame() %>% 
               group_by(lev) %>%
               by(.$lev, function(x) apply(x[, -ncol(.)], 2, pandoc.pcts)) %>% 
               do.call(rbind, .)) %>% 
      do.call(rbind, .)
    facs <- fac.values %>% 
      rbind(fac.values.tot) %>% 
      extract(, match(c(by1, by2), colnames(.))) %>% 
      set_rownames(NULL) %>% 
      as.data.frame()
    fac.result <- fac.values %>% 
      extract(, -match(c(by1, by2), colnames(.))) %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate(grp = stringr::str_split_fixed(rownames(.), "\\.", 2)[, 1]) %>% 
      group_by(grp) %>% 
      do(vars = rowColPercent(.[, -ncol(.)])) %>%
      extract(match(.$grp, fac.var), ) %>% 
      use_series(vars) %>% 
      lapply(function(x) x %>% 
               cbind(lev = rep(letters[1:(nrow(x) / 3)], each = 3)) %>% 
               as.data.frame() %>% 
               group_by(lev) %>%
               by(.$lev, function(x) apply(x[, -ncol(.)], 2, pandoc.pcts)) %>% 
               do.call(rbind, .)) %>% 
      do.call(rbind, .) %>% 
      cbind(fac.values.res) %>% 
      rbind(t(facs), .) %>% 
      extract(, order(.[by1, ])) %>% 
      set_rownames(colnames(fac.values.tot)) %>%
      rbind(matrix(rep("", nrow(facs) * length(fac.var)),
                   nrow = length(fac.var), dimnames = list(fac.var, NULL))) %>%
      extract(row.order, ) %>% 
      set_rownames(stringr::str_replace_all(
        rownames(.),
        c(setNames(c(rep("", length(fac.var))), paste0(fac.var, ".")),
          setNames(paste0("**", fac.var, "**"), fac.var)))) %>% 
      set_colnames(mapply(function(x, y) paste(x, y, sep = "="),
                          names(facs), facs[order(facs[, by1]), ]) %>% 
                     apply(., 1, function(x) paste(x, collapse = ", ")) %>%
                     ifelse(grepl("NA", .),
                            stringr::str_split_fixed(., ", ", 2)[, 1], .))
    fac.values.long <- fac.values %>% 
      as.data.frame() %>% 
      tidyr::gather(stat, value, -match(c(by1, by2), colnames(.))) %>% 
      tidyr::separate(stat, c("var", "stat"), "\\.") %>%
      extract(order(.[, "var"], .[, by1], .[, by2]), ) %>% 
      set_rownames(NULL)
  } else {
    fac.result <- fac.values.long <- NULL
  }
  final.values.long <- rbind(num.values.long, fac.values.long) %>% 
    extract(order(match(.$var, var.names)), )
  final.result <- rbind(num.result, fac.result)
  fin.html <- final.result %>% 
    set_rownames(stringr::str_replace_all(
      rownames(.), c("^\\*\\*" = "<b>", "\\*\\*$" = "</b>")))
  ind <- grep("\\*", rownames(final.result))
  org.ord <- gsub("\\*\\*", "", rownames(final.result)[ind])
  fin <- final.result %>% 
    extract(mapply(rep, match(org.ord, var.names),
                   diff(c(ind, nrow(.) + 1))) %>% 
              unlist() %>% 
              factor() %>% 
              order(), )
  format <- match.arg(format)
  return(switch(format,
                pandoc = pander::pandoc.table.return(
                  fin, emphasize.rownames = F),
                html = htmlTable::htmlTable(fin.html),
                long = final.values.long))
}
