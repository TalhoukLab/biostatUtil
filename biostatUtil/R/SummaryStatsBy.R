#' Generate summary statistics
#' 
#' Splits the data by two variables, computing relevant statistics for each 
#' variable.
#' 
#' The \code{data} is split by two variables, \code{by1} and \code{by2}, and 
#' statistics are computed for continuous variables. Statistics currently 
#' supported include \code{ mean, sd, median, IQR, range}, and the number of 
#' missing cases. For factor variables, the counts, column and row percentages 
#' are shown for each of the variable levels.
#' 
#' Note that marginal statistics are also shown for \code{by1}, so the order in 
#' which you split \code{data} matters.
#' 
#' There are four print options for the output: \code{raw} gives the output as a
#' character matrix, \code{pandoc} gives a Pandoc-friendly output for Word and 
#' PDF reports, \code{html} gives HTML supported output, and \code{long} is a 
#' tidy version of \code{raw}.
#' 
#' @param data The \code{data.frame} containing the data
#' @param by1 character string of splitting variable 1
#' @param by2 character string of splitting variable 2
#' @param var.names character vector of variables to compute statistics for
#' @param stats statistics to compute for continuous variables
#' @param digits number of digits to round to
#' @param format format to return the table in. Either "raw", "pandoc" (for Word
#'   and PDF), "html", or "long" format for graphing and data manipulation using
#'   raw values.
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples 
#' mtcars$vs <- as.factor(mtcars$vs); mtcars$am <- as.factor(mtcars$am)
#' SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg", "vs", "qsec", "am"))
#' SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("vs", "qsec"))
#' SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg"))
SummaryStatsBy <- function(data, by1, by2, var.names,
                           stats = c("mean", "sd", "median", "IQR", "range",
                                     "missing"), digits = 3,
                           format = c("raw", "pandoc", "html", "long")) {
  . <- stat <- value <- grp <- vars <- lev <- NULL
  var.dat <- data[, var.names, drop = FALSE]
  types <- sapply(var.dat, class)
  num.ind <- types %in% c("numeric", "integer")
  fac.ind <- types %in% c("factor", "character")
  if (!(num.ind || fac.ind)) {
    stop('Variables must be numeric, integer, factor, or character.')
  }
  if (sum(num.ind) > 0) {
    num.var <- var.names[num.ind]
    num.dat <- structure(data.frame(var.dat[num.var], data[, c(by1, by2)]),
                         names = c(num.var, by1, by2))
  } else {
    num.var <- num.dat <- NULL
  }
  if (sum(fac.ind) > 0) {
    fac.var <- var.names[fac.ind]
    fac.dat <- structure(data.frame(var.dat[fac.var], data[, c(by1, by2)]),
                         names = c(fac.var, by1, by2))
  } else {
    fac.var <- fac.dat <- NULL
  }
  col.names <- unique(data[, c(by1, by2)]) %>% 
    rbind(matrix(c(unique(data[, by1]), rep(NA, n_distinct(data[, by1]))),
                 ncol = 2, dimnames = list(NULL, c(by1, by2)))) %>% 
    arrange_(by1, by2) %>%
    mapply(function(x, y) paste(x, y, sep = "="), names(.), .) %>% 
    apply(., 1, paste, collapse = ", ") %>% 
    ifelse(grepl("NA", .), stringr::str_split_fixed(., ", ", 2)[, 1], .)
  
  # Compute numerical summaries
  if (!is.null(num.var)) {
    num.ord <- unlist(lapply(num.var, function(x)
      paste0(x, c("", paste0(".", stats)))))
    if (all(c("mean", "sd") %in% stats)) {
      stats <- stats[-grep("sd", stats)]
      num.ord <- num.ord[-grep("sd", num.ord)]
    }
    num.val <- doBy::summaryBy(as.formula(
      paste(paste(num.var, collapse = " + "), "~", paste(by1, by2, sep = " + "))),
      num.dat, FUN = contSumFunc, digits = digits, stats = stats) %>% 
      mutate_each(funs(as.character))
    num.val.tot <- doBy::summaryBy(as.formula(
      paste(paste(num.var, collapse = " + "), "~", by1)),
      num.dat, FUN = contSumFunc, digits = digits, stats = stats) %>% 
      mutate_each(funs(as.character))
    num.all <- data.table::rbindlist(list(num.val, num.val.tot), fill = TRUE)
    num.long <- num.all %>% 
      tidyr::gather(stat, value, -match(c(by1, by2), names(.))) %>%
      tidyr::separate(stat, c("var", "stat"), "\\.") %>%
      arrange_(by1, by2)
    num.res <- matrix(num.long$value, ncol = nrow(num.all),
                      dimnames = list(grep(paste(num.var, collapse = "|"),
                                           names(num.val), value = TRUE),
                                      col.names)) %>% 
      rbind(matrix("", nrow = length(num.var), ncol = nrow(num.all),
                   dimnames = list(num.var))) %>% 
      extract(num.ord, ) %>% 
      set_rownames(unlist(lapply(paste0("**", num.var, "**"),
                                 function(x) c(x, stats))))
  } else {
    num.res <- num.long <- NULL
  }
  
  # Compute factor summaries
  if (!is.null(fac.var)) {
    fac.ord <- c(mapply(function(x, y) paste0(x, c("", paste0(".", levels(y)))),
                        fac.var, fac.dat[, fac.var, drop = FALSE]))
    fac.val <- sapply(fac.var, function(x)
      as.formula(paste(x, "~", paste(by1, by2, sep = " + ")))) %>%
      lapply(function(y) as.matrix(aggregate(y, fac.dat, summary))) %>% 
      Reduce(merge, .) %>% 
      as.data.frame()
    fac.val.tot <- sapply(fac.var, function(x)
      as.formula(paste(x, "~", paste(by1, sep = " + ")))) %>%
      lapply(function(y) as.matrix(aggregate(y, fac.dat, summary))) %>% 
      Reduce(merge, .) %>% 
      as.data.frame()
    fac.all <- data.table::rbindlist(list(fac.val, fac.val.tot), fill = TRUE) %>%
      as.data.frame()
    fac.pct <- fac.val %>% 
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
      do.call(rbind, .)
    fac.pct.tot <- fac.val.tot %>% 
      split(.[, by1]) %>% 
      lapply(function(x) x %>% 
               select(-matches(by1)) %>% 
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
    fac.res <- cbind(fac.pct, fac.pct.tot) %>% 
      set_rownames(tail(names(fac.val), -2)) %>%
      rbind(matrix(rep("", nrow(fac.all) * length(fac.var)),
                   nrow = length(fac.var), dimnames = list(fac.var, NULL))) %>%
      extract(fac.ord, order(fac.all[, by1])) %>% 
      set_rownames(stringr::str_replace_all(
        rownames(.),
        c(setNames(c(rep("", length(fac.var))), paste0(fac.var, ".")),
          setNames(paste0("**", fac.var, "**"), fac.var)))) %>% 
      set_colnames(col.names)
    fac.long <- fac.all %>% 
      tidyr::gather(stat, value, -match(c(by1, by2), colnames(.))) %>% 
      tidyr::separate(stat, c("var", "stat"), "\\.") %>%
      arrange_(by1, by2)
  } else {
    fac.res <- fac.long <- NULL
  }

  # Final results in each format
  final.res <- rbind(num.res, fac.res)
  ind <- grep("\\*", rownames(final.res))
  org.ord <- gsub("\\*\\*", "", rownames(final.res)[ind])
  final.reord <- final.res %>% 
    extract(order(unlist(mapply(rep, match(org.ord, var.names),
                   diff(c(ind, nrow(.) + 1))))), )
  final.html <- final.res %>% 
    set_rownames(stringr::str_replace_all(
      rownames(.), c("^\\*\\*" = "<b>", "\\*\\*$" = "</b>")))
  final.long <- rbind(num.long, fac.long) %>% 
    extract(order(match(.$var, var.names)), )
  final.return <- switch(match.arg(format),
                         raw = final.reord,
                         pandoc = pander::pandoc.table.return(
                           final.reord, emphasize.rownames = FALSE),
                         html = htmlTable::htmlTable(final.html),
                         long = final.long)
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