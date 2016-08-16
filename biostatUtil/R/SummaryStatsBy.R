#' Generate cohort characteristics
#' 
#' Splits the data by two variables, computing relevant statistics for each variable.
#' 
#' @param data The \code{data.frame} containing the data
#' @param by1 split variable 1
#' @param by2 split variable 2
#' @param var.names The variables that you want the statistics for
#' @param stats statistics to show compute for continuous variables
#' @param marker.description The description for the variable(s) to split
#' @param groups.description The groupings of the marker
#' @param var.descriptions descriptions of \code{var.names}
#' @param digits number of digits to round to
#' @param format format to return the table in. Either "raw", "pandoc" (for Word and PDF),
#' "html", or "long" format for graphing and data manipulation using raw values.
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples 
#' mtcars$vs <- as.factor(mtcars$vs); mtcars$am <- as.factor(mtcars$am)
#' SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg", "vs", "qsec", "am"))
#' SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("vs", "qsec"))
#' SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg"))
SummaryStatsBy <- function(data, by1, by2, var.names,
                           stats = c("mean", "sd", "median", "IQR", "range",
                                     "missing"),
                           marker.description = by1, groups.description = by2,
                           var.descriptions = var.names, digits = 3,
                           format = c("raw", "pandoc", "html", "long")) {
  . <- stat <- value <- grp <- vars <- lev <- NULL
  var.dat <- data[, var.names, drop = FALSE]
  facets <- data[, c(by1, by2)]
  types <- sapply(var.dat, class)
  num.ind <- types %in% c("numeric", "integer")
  fac.ind <- types %in% c("factor", "character")
  if (!(num.ind || fac.ind)) {
    stop('Variables must be numeric, integer, factor, or character.')
  }
  if (sum(num.ind) > 0) {
    num.var <- var.names[num.ind]
    num.dat <- structure(data.frame(var.dat[num.var], facets),
                         names = c(num.var, by1, by2))
  } else {
    num.var <- num.dat <- NULL
  }
  if (sum(fac.ind) > 0) {
    fac.var <- var.names[fac.ind]
    fac.dat <- structure(data.frame(var.dat[fac.var], facets),
                         names = c(fac.var, by1, by2))
  } else {
    fac.var <- fac.dat <- NULL
  }
  
  row.order <- unlist(lapply(num.var, function(x)
    paste0(x, c("", paste0(".", stats)))))
  if (all(c("mean", "sd") %in% stats)) {
    stats <- stats[-grep("sd", stats)]
    row.order <- row.order[-grep("sd", row.order)]
  }
  
  # Compute numerical summaries
  if (!is.null(num.var)) {
    num.form <- as.formula(paste(paste(num.var, collapse = " + "), "~",
                                 paste(by1, by2, sep = " + ")))
    num.form.tot <- as.formula(paste(paste(num.var, collapse = " + "), "~", by1))
    num.values <- doBy::summaryBy(num.form, num.dat, FUN = contSumFunc,
                                  digits = digits, stats = stats) %>% 
      mutate_each(funs(as.character))
    num.values.tot <- doBy::summaryBy(num.form.tot, num.dat, FUN = contSumFunc,
                                      digits = digits, stats = stats) %>% 
      cbind(matrix(NA, dimnames = list(NULL, by2))) %>% 
      extract(, c(1, ncol(.), 2:(ncol(.) - 1))) %>% 
      rbind(num.values, .) %>% 
      arrange_(by1)
    num.values.long <- num.values.tot %>% 
      tidyr::gather(stat, value, -match(c(by1, by2), names(.))) %>%
      tidyr::separate(stat, c("var", "stat"), "\\.") %>%
      arrange_(by1, by2)
    facs <- num.values.tot[, c(by1, by2)]
    num.result <- matrix(num.values.long$value, ncol = nrow(facs)) %>% 
      rbind(matrix("", nrow = length(num.var), ncol = nrow(facs))) %>% 
      set_rownames(c(grep(paste(num.var, collapse = "|"), names(num.values),
                          value = TRUE), num.var)) %>% 
      extract(row.order, ) %>% 
      set_rownames(unlist(lapply(paste0("**", num.var, "**"),
                                 function(x) c(x, stats)))) %>% 
      set_colnames(mapply(function(x, y) paste(x, y, sep = "="),
                          names(facs), facs[order(facs[, by1]), ]) %>%
                     apply(., 1, function(x) paste(x, collapse = ", ")) %>% 
                     ifelse(grepl("NA", .), 
                            stringr::str_split_fixed(., ", ", 2)[, 1], .))
  } else {
    num.result <- num.values.long <- NULL
  }
  
  # Compute factor summaries
  if (!is.null(fac.var)) {
    row.order <- fac.var %>% 
      mapply(function(x, y) paste0(x, ".", levels(y)), ., fac.dat[, ., drop = FALSE]) %>%
      rbind(colnames(.), .) %>% 
      c()
    fac.values <- fac.var %>% 
      sapply(function(x) as.formula(paste(x, "~",
                                          paste(by1, by2, sep = " + ")))) %>%
      lapply(aggregate, fac.dat, summary) %>% 
      Reduce(merge, .) %>% 
      extract(order(.[, by1]), ) %>% 
      as.matrix() %>% 
      as.data.frame()
    fac.values.tot <- fac.var %>% 
      sapply(function(x) as.formula(paste(x, "~",
                                          paste(by1, sep = " + ")))) %>%
      lapply(aggregate, fac.dat, summary) %>% 
      Reduce(merge, .) %>% 
      extract(order(.[, by1]), ) %>% 
      as.matrix() %>% 
      as.data.frame() %>% 
      cbind(matrix(rep(NA, nrow(.)), dimnames = list(NULL, by2))) %>% 
      extract(, c(1, ncol(.), 2:(ncol(.) - 1)))
    fac.values.res <- fac.values.tot %>% 
      split(.[, by1]) %>% 
      lapply(function(x) x %>% 
               select(-one_of(by1, by2)) %>% 
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
               by(.$lev, function(x) apply(x[, -ncol(.)], 2, pandoc_pcts)) %>% 
               do.call(rbind, .)) %>% 
      do.call(rbind, .)
    facs <- rbind(fac.values, fac.values.tot, make.row.names = FALSE) %>%
      select_(by1, by2)
    fac.result <- fac.values %>% 
      select(-one_of(by1, by2)) %>% 
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
               by(.$lev, function(x) apply(x[, -ncol(.)], 2, pandoc_pcts)) %>% 
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
    fac.values.long <- fac.values.tot %>% 
      as.data.frame() %>% 
      tidyr::gather(stat, value, -match(c(by1, by2), colnames(.))) %>% 
      tidyr::separate(stat, c("var", "stat"), "\\.") %>%
      arrange_(by1, by2)
  } else {
    fac.result <- fac.values.long <- NULL
  }

  # Final results in each format
  final.result <- rbind(num.result, fac.result)
  ind <- grep("\\*", rownames(final.result))
  org.ord <- gsub("\\*\\*", "", rownames(final.result)[ind])
  
  final.pandoc <- final.result %>% 
    extract(mapply(rep, match(org.ord, var.names),
                   diff(c(ind, nrow(.) + 1))) %>% 
              unlist() %>% 
              factor() %>% 
              order(), )
  final.html <- final.result %>% 
    set_rownames(stringr::str_replace_all(
      rownames(.), c("^\\*\\*" = "<b>", "\\*\\*$" = "</b>")))
  final.values.long <- rbind(num.values.long, fac.values.long) %>% 
    extract(order(match(.$var, var.names)), )
  final.return <- switch(match.arg(format),
                         raw = final.result,
                         pandoc = pander::pandoc.table.return(
                           final.pandoc, emphasize.rownames = FALSE),
                         html = htmlTable::htmlTable(final.html),
                         long = final.values.long)
  return(final.return)
}

#' Formatting percentages for Pandoc
#' @noRd
pandoc_pcts <- function(char) {
  count <- as.integer(char[1])
  pcts <- as.numeric(char[-1]) * 100
  if (length(char) > 2)
    return(paste0(count, " (", pcts[1], "%, ", pcts[2], "%)"))
  else
    return(paste0(count, " (", pcts, "%)"))
}

#' Continuous summary functions
#' @noRd
contSumFunc <- function(x, digits, stats = c("mean", "sd", "median", "IQR",
                                             "range", "missing")) {
  stats.choices <- c("mean", "sd", "median", "IQR", "range", "missing")
  stats.arg <- match.arg(stats, stats.choices, several.ok = TRUE)
  funs.arg <- stats.arg
  if ("missing" %in% stats)
    funs.arg[match("missing", funs.arg)] <- "n_missing"
  all.stats <- mapply(function(c, f) match_fun_null(x, c, stats.choices,
                                                    f, na.rm = TRUE),
                      c = stats.arg, f = funs.arg) %>%
    sapply(function(x) {
      r <- as.character(round(x, digits = digits))
      ifelse(length(r) > 1, paste(r, collapse = "-"), r)
    })
  if (all(c("mean", "sd") %in% stats)) {
    all.stats["mean"] <- paste(all.stats["mean"], all.stats["sd"],
                               sep = " &#177; ")
    all.stats <- all.stats[-match("sd", names(all.stats))]
  }
  return(all.stats)
}

#' If match found, apply function, otherwise NULL
#' @noRd
match_fun_null <- function(x, case, table, FUN, ...) {
  if (case %in% table) {
    out <- do.call(FUN, c(list(x), ...))
  } else {
    out <- NULL
  }
  return(out)
}

#' Count number of missing elements
#' @noRd
n_missing <- function(x, na.rm = FALSE) {
  return(sum(is.na(x), na.rm = na.rm))
}
