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
  stat <- value <- grp <- vars <- lev <- NULL
  stats <- match.arg(stats, c("mean", "sd", "median", "IQR", "range",
                              "missing"), several.ok = TRUE)
  var.dat <- data[, var.names, drop = FALSE]
  types <- purrr::map_chr(var.dat, class)
  num.ind <- types %in% c("numeric", "integer")
  fac.ind <- types %in% c("factor", "character")
  if (!(num.ind || fac.ind)) {
    stop('Variables must be numeric, integer, factor, or character.')
  }
  assertthat::assert_that(n_distinct(data[, by1]) >= 2,
                          n_distinct(data[, by2]) >= 2)
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
    purrr::map2(names(.), ., ~ paste(.x, .y, sep = "=")) %>% 
    purrr::pmap_chr(paste, sep = ", ") %>% 
    ifelse(grepl("NA", .), stringr::str_split_fixed(., ", ", 2)[, 1], .)
  
  # Compute numerical summaries
  if (!is.null(num.var)) {
    num.ord <- unlist(lapply(num.var, function(x)
      paste0(x, c("", paste0(".", stats)))))
    if (all(c("mean", "sd") %in% stats)) {
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
    num.all <- dplyr::bind_rows(list(num.val, num.val.tot))
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
      magrittr::extract(num.ord, ) %>% 
      magrittr::set_rownames(num.ord %>%
                               ifelse(!grepl("\\.", .),
                                      paste0("**", ., "**"), .) %>%
                               gsub(".+\\.", "\\1", .))
  } else {
    num.res <- num.long <- NULL
  }
  
  # Compute factor summaries
  if (!is.null(fac.var)) {
    fac.ord <- purrr::map2(fac.var, fac.dat[, fac.var, drop = FALSE],
                            ~ paste0(.x, c("", paste0(".", levels(.y))))) %>% 
      unlist()
    fac.val <- fac.var %>% 
      purrr::map(~ as.formula(paste(.x, "~", paste(by1, by2, sep = " + ")))) %>%
      purrr::map(~ as.matrix(aggregate(.x, fac.dat, summary))) %>% 
      Reduce(merge, .) %>% 
      magrittr::set_colnames(c(by1, by2, grep("\\.", fac.ord,
                                              value = TRUE))) %>% 
      as.data.frame()
    fac.val.tot <- fac.var %>% 
      purrr::map(~ as.formula(paste(.x, "~", paste(by1, sep = " + ")))) %>%
      purrr::map(~ as.matrix(aggregate(.x, fac.dat, summary))) %>% 
      Reduce(merge, .) %>% 
      magrittr::set_colnames(c(by1, grep("\\.", fac.ord, value = TRUE))) %>% 
      as.data.frame()
    fac.all <- dplyr::bind_rows(list(fac.val, fac.val.tot))    
    fac.pct <- fac.val %>% 
      select(-one_of(by1, by2)) %>% 
      t() %>% 
      as.data.frame() %>% 
      mutate(grp = stringr::str_split_fixed(rownames(.), "\\.", 2)[, 1]) %>% 
      group_by(grp) %>% 
      do(vars = rowColPercent(.[, -ncol(.)])) %>%
      magrittr::extract(order(match(.$grp, fac.var)), ) %>% 
      magrittr::use_series(vars) %>% 
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
      magrittr::set_rownames(tail(names(fac.val), -2)) %>%
      rbind(matrix(rep("", nrow(fac.all) * length(fac.var)),
                   nrow = length(fac.var), dimnames = list(fac.var, NULL))) %>%
      magrittr::extract(fac.ord, order(fac.all[, by1])) %>% 
      magrittr::set_rownames(stringr::str_replace_all(
        rownames(.),
        c(setNames(c(rep("", length(fac.var))), paste0(fac.var, ".")),
          setNames(paste0("**", fac.var, "**"), fac.var)))) %>% 
      magrittr::set_colnames(col.names)
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
    magrittr::extract(order(unlist(mapply(rep, match(org.ord, var.names),
                                          diff(c(ind, nrow(.) + 1))))), )
  final.html <- final.res %>% 
    magrittr::set_rownames(stringr::str_replace_all(
      rownames(.), c("^\\*\\*" = "<b>", "\\*\\*$" = "</b>")))
  final.long <- rbind(num.long, fac.long) %>% 
    magrittr::extract(order(match(.$var, var.names)), )
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
  funs.arg <- match.arg(stats, stats.choices, several.ok = TRUE)
  if ("missing" %in% stats)
    funs.arg[match("missing", funs.arg)] <- "n_missing"
  all.stats <- purrr::map_chr(funs.arg, ~ {
    match_fun_null(x = x, .x, na.rm = TRUE) %>% 
      round(., digits = digits) %>% 
      as.character() %>% 
      ifelse(length(.) > 1, paste(., collapse = "-"), .)
  }) %>%
    magrittr::set_names(stats)
  if (all(c("mean", "sd") %in% stats)) {
    all.stats["mean"] <- paste(all.stats[c("mean", "sd")],
                               collapse = " &#177; ")
    all.stats <- all.stats[-match("sd", names(all.stats))]
  }
  return(all.stats)
}

#' Apply function on every element of list
#' @noRd
match_fun_null <- function(x, FUN, ...) {
  return(do.call(FUN, c(list(x), ...)))
}