#' Fit a Cox proportional hazards regression model
#'
#' Fits `coxph` on all survival endpoints. `doCoxphGeneric` fits univariable
#' models for each variable in `var.names`, and `doCoxphMultivariable` fits a
#' multivariable model for all variables together in `var.names`.
#'
#' Please note the following assumptions:
#' * Marker can be binary, continuous or categorical.
#' * Missing survival time/status variables are coded as `NA` (i.e. will only be
#' checked by `is.na()`).
#' * Survival time/status variable name specified in the following order: "os",
#' "dss", "rfs".
#' * Coding of survival status is binary only (i.e. cannot take survival status
#' of > 2 categories).
#'
#' @param input.d The `data.frame` containing the data
#' @param var.names variables to include as predictors
#' @param var.descriptions vector of strings to describe the variables as they
#'   are to appear in the table
#' @param show.var.detail logical. If `TRUE`, details such as categories and the
#'   reference group for categorical variables are shown.
#' @param show.group.name.for.bin.var logical. If `TRUE`, the non-reference
#'   group name is shown beside the hazard ratio for dichotomized variables.
#' @param var.ref.groups a vector of reference groups. If `NULL`, assume all
#'   variables are binary/continuous. If an item in the vector is `NA`, assume
#'   that particular marker is binary or continuous (i.e., treat it as a numeric
#'   variable)
#' @param var.names.surv.time variable names of survival time
#' @param var.names.surv.time2 optional variable names for interval ending
#'   times, used when survival is left-truncated. The interpretation of
#'   `var.names.surv.time` becomes interval starting times when this argument is
#'   not `NULL`.
#' @param var.names.surv.status variable names of survival status
#' @param event.codes.surv event coding of survival status variable
#' @param surv.descriptions names abbreviated survival endpoints in returned
#'   output
#' @param var.strata optional variable name to stratify by using
#'   [survival::strata()]
#' @param missing.codes character strings of missing values used in `input.d`
#' @param use.firth percentage of censored cases before using Firth's method for
#'   Cox regression. If `use.firth = 1` (default), Firth is never used and if
#'   `use.firth = -1` Firth is always used.
#' @param firth.caption subscript in html table output indicating Firth was used
#' @param add_log_hr if `TRUE`, show the log hazard ratio
#' @param stat.test the overall model test to perform on the Cox regression
#'   model. Can be any of "waldtest", "logtest", or "sctest". If Firth is used,
#'   only "logtest" can be performed. Test p-values are never Firth corrected (
#'   per instruction from Aline 2015-04-14,15).
#' @param bold_pval logical; if `TRUE`, p-values are bolded if statistically
#'   significant at `sig.level`
#' @param sig.level significance level; default 0.05
#' @param round.digits.hr number of digits for hazard ratio
#' @param round.digits.p.value number of digits for p-value
#' @param round.small if `TRUE`, uses small number rounding via [round_small()]
#'   for p-values
#' @param scientific if `TRUE`, uses scientific notation when rounding
#' @param caption caption for returned object
#' @param html.table.border the border type to use for html tables
#' @param banded.rows logical. If `TRUE`, rows have alternating shading colour
#' @param css.class.name.odd Used to set the row colour for odd rows
#' @param css.class.name.even Used to set the row colour for even rows
#' @param split.table number of characters per row before splitting the table.
#'   Applies to the pandoc table output.
#' @param format Either a "long" or "wide" format for the `result.table.bamboo`
#'   result element
#' @param ... additional arguments to [pander::pandoc.table.return()]
#' @return A list with the following elements
#' * `result.table`: a data frame with the Cox model results.
#' * `result.table.bamboo`: results transformed into a pandoc-ready format.
#' Display using `cat()`.
#' * `result.table.html`: results transformed into an HTML-ready format. Display
#' using [htmlTable::htmlTable()].
#' * `cox.stats`: present for `doCoxphMultivariable()` only. A list of
#' additional Cox model output statistics.
#' @name doCoxph
#' @author Samuel Leung, Aline Talhouk, Derek Chiu
#' @export
#' @examples
#' library(survival)
#' doCoxphGeneric(input.d = lung, var.names = "sex", var.descriptions = "Sex",
#'                var.names.surv.time = "time",
#'                var.names.surv.status = "status", event.codes.surv = "2",
#'                surv.descriptions = "OS", caption = "")
doCoxphGeneric <- function(
  input.d, var.names, var.descriptions, show.var.detail = FALSE,
  show.group.name.for.bin.var = FALSE, var.ref.groups = NULL,
  var.names.surv.time = c("os.yrs", "dss.yrs", "rfs.yrs"),
  var.names.surv.time2 = NULL,
  var.names.surv.status = c("os.sts", "dss.sts", "rfs.sts"),
  event.codes.surv = c("os.event", "dss.event", "rfs.event"),
  surv.descriptions = c("OS", "DSS", "PFS"),
  var.strata = NULL,
  missing.codes = c("N/A", "", "Unk"),
  use.firth = 1, firth.caption = FIRTH.CAPTION, add_log_hr = FALSE,
  stat.test = "waldtest", bold_pval = FALSE, sig.level = 0.05,
  round.digits.hr = 2,
  round.digits.p.value = 4, round.small = FALSE, scientific = FALSE,
  caption = NA, html.table.border = 0, banded.rows = FALSE,
  css.class.name.odd = "odd", css.class.name.even = "even",
  split.table = 300, format = c("long", "wide"), ...) {

  # Constants
  kLocalConstantHrSepFlag <- "kLocalConstantHrSepFlag" # separates HR estimates
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE

  # Initial assertion checks
  num.surv.endpoints <- length(var.names.surv.time)
  assertthat::assert_that(num.surv.endpoints == length(var.names.surv.status),
                          num.surv.endpoints == length(event.codes.surv),
                          num.surv.endpoints == length(surv.descriptions))
  if (!is.null(var.names.surv.time2)) {
    assertthat::assert_that(num.surv.endpoints == length(var.names.surv.time2))
  }

  # Ensure variable names are not named vectors
  var.names <- unname(var.names)
  var.names.surv.time <- unname(var.names.surv.time)
  var.names.surv.time2 <- unname(var.names.surv.time2)
  var.names.surv.status <- unname(var.names.surv.status)
  var.strata <- unname(var.strata)

  # Remove all variables not used in analysis, ensure survival times are numeric
  input.d <- input.d %>%
    dplyr::select(dplyr::all_of(c(
      var.names,
      var.names.surv.time,
      var.names.surv.time2,
      var.names.surv.status,
      var.strata
    ))) %>%
    droplevels() %>%
    dplyr::mutate_at(c(var.names.surv.time, var.names.surv.time2), as.numeric)

  # Setup default for variable reference groups and result matrix
  nvar <- length(var.names)
  var.ref.groups <- var.ref.groups %||% rep(NA, nvar)
  rn <- paste(rep(var.names, each = num.surv.endpoints), surv.descriptions,
              sep = "-")
  cn <- c("# of events / n", "Hazard Ratio (95% CI)",
          paste0(ifelse(stat.test == "logtest", "LRT ", ""), "P-value"))
  if (add_log_hr) {
    cn <- append(cn, "Log Hazard Ratio", 1)
  }
  result.table <- matrix(
    NA_character_,
    nrow = nvar * num.surv.endpoints,
    ncol = length(cn),
    dimnames = list(rn, cn)
  )
  for (i in seq_along(var.names)) {
    x <- var.names[i]
    temp.d <- input.d %>%  # remove any cases with NA's or missing values
      dplyr::filter(!is.na(.[[x]]) & !(.[[x]] %in% missing.codes))
    # automatically set ref.group to lowest group if not specified
    if (is.factor(temp.d[[x]]) & is.na(var.ref.groups[i])) {
      var.ref.groups[i] <- names(table(temp.d[[x]]))[1]
    }
    if (is.na(var.ref.groups[i])) {
      temp.d[[x]] <- as.numeric(temp.d[[x]])
    } else {
      temp.d[[x]] <- stats::relevel(as.factor(temp.d[[x]]), var.ref.groups[i])
    }

    for (j in seq_len(num.surv.endpoints)) {
      surv.formula <- surv_formula(
        time = var.names.surv.time[j],
        status = var.names.surv.status[j],
        event = event.codes.surv[j],
        terms = x,
        time2 = var.names.surv.time2[j],
        strata = var.strata
      )
      temp.d.no.missing.survival <- temp.d %>%
        dplyr::filter(!is.na(.[[var.names.surv.status[[j]]]]),
                      !is.na(.[[var.names.surv.time[[j]]]]))
      cox.stats <- prettyCoxph(surv.formula,
                               input.d = temp.d.no.missing.survival,
                               use.firth = use.firth)
      e.n <- paste(cox.stats$nevent, "/", cox.stats$n)
      hr.ci <- cox.stats$output %>%
        magrittr::extract(, c("estimate", "conf.low", "conf.high")) %>%
        format_hr_ci(digits = round.digits.hr, labels = FALSE, method = "Sci") %>%
        paste0(ifelse(cox.stats$used.firth, firth.caption, "")) %>%
        paste(collapse = kLocalConstantHrSepFlag)
      pval <- summary(cox.stats$fit)[[stat.test]][["pvalue"]]
      pval_f <- pval %>%
        round_pval(round.small = round.small,
                   scientific = scientific,
                   digits = round.digits.p.value) %>%
        ifelse(bold_pval & pval < sig.level, paste0("**", ., "**"), .)
      res <- c(e.n, hr.ci, pval_f)
      if (add_log_hr) {
        log_hr <- cox.stats$output %>%
          magrittr::extract(, "estimate") %>%
          log() %>%
          round(digits = round.digits.hr) %>%
          paste0(ifelse(cox.stats$used.firth, firth.caption, "")) %>%
          paste(collapse = kLocalConstantHrSepFlag)
        res <- append(res, log_hr, 1)
      }
      result.table[num.surv.endpoints * (i - 1) + j, ] <- res
    }
  }

  ### generate html table ... ###
  result.table.html <- paste0("<table border=", html.table.border, ">",
                              ifelse(is.na(caption), "",
                                     paste0("<caption style='",
                                            TABLE.CAPTION.STYLE, "'>", caption,
                                            "</caption>")),
                              "<tr><th style='", col.th.style,
                              "' colspan=2></th><th style='", col.th.style,
                              "'>",
                              paste(colnames(result.table),
                                    collapse = paste0("</th><th style='",
                                                      col.th.style, "'>")),
                              "</th></tr>")
  # print values
  i <- 1
  while (i <= nrow(result.table)) {
    is.first.row <- TRUE
    tr.class <- ifelse(banded.rows,
                       paste0(" class='",
                              ifelse((floor(i / num.surv.endpoints) + 1) %%
                                       2 == 0, css.class.name.even,
                                     css.class.name.odd), "'"), "")
    var.index <- floor((i - 1) / num.surv.endpoints) + 1
    var.description <- var.descriptions[var.index]
    if (show.var.detail) {
      var.categories <- names(table(input.d[, var.names[var.index]]))
      var.categories <- var.categories[!var.categories %in% missing.codes]
      if (!is.na(var.ref.groups[var.index])) {
        # this variable is categorical ... print ref group and categories
        var.description <- paste0(var.description, ":<br><br><i>",
                                  paste(var.categories, collapse = "<br>"),
                                  "<br><br>(reference group:", var.ref.groups[var.index], ")</i>")
      }
    }
    result.table.html <- paste0(result.table.html, "<tr", tr.class,
                                "><th style='", row.th.style, "' rowspan=",
                                num.surv.endpoints, ">",
                                var.description, "</th>")
    for (surv.description in surv.descriptions) {
      result.table.html <- paste0(
        result.table.html,
        ifelse(is.first.row, "", paste0("<tr", tr.class, ">")),
        "<th style='", row.th.style, "'>", surv.description, "</th><td>",
        paste(gsub(kLocalConstantHrSepFlag, "<br>", result.table[i, ]), collapse = "</td><td>"), "</td></tr>")

      is.first.row <- FALSE # if run any time after the first row, must not be the first row any more
      i <- i + 1
    }
  }
  result.table.html <- paste0(result.table.html, "</table>")
  ### end of generate html table ###

  ### generate word-friendly table via pander i.e. result.table.bamboo ... ###
  result.table.bamboo <- result.table
  result.table.ncol <- ncol(result.table)
  result.table.bamboo.base.indexes <- c() # base indexes for each variable in result.table.bamboo
  # want to add empty rows for var description
  for (var.count in seq_along(var.names)) {
    result.table.bamboo.base.index <- 1 + (var.count - 1) * (length(surv.descriptions) + 1)
    if (var.count == 1) {
      result.table.bamboo <- rbind(rep("", result.table.ncol), result.table.bamboo)
    } else {
      result.table.bamboo <- rbind(
        result.table.bamboo[1:(result.table.bamboo.base.index - 1), ],
        rep("", result.table.ncol),
        result.table.bamboo[result.table.bamboo.base.index:nrow(result.table.bamboo), ])
    }
    rownames(result.table.bamboo)[result.table.bamboo.base.index] <- paste0("**", var.descriptions[var.count],
                                                                            ifelse(show.var.detail,
                                                                                   paste0(" (reference group:", var.ref.groups[var.count], ")"),
                                                                                   ""), "**")
    rownames(result.table.bamboo)[result.table.bamboo.base.index + c(1:length(surv.descriptions))] <- surv.descriptions
    result.table.bamboo.base.indexes <- c(result.table.bamboo.base.indexes, result.table.bamboo.base.index)
  }
  # want to add a column to describe different factor level for categorical
  # whenever reference group is specified
  if (sum(is.na(var.ref.groups)) != length(var.ref.groups)) {
    first.col.name <- colnames(result.table.bamboo)[1]
    result.table.bamboo <- cbind(result.table.bamboo[, 1], "",
                                 result.table.bamboo[, -1])
    colnames(result.table.bamboo)[1] <- first.col.name
    # column with the hazard ratios
    if (add_log_hr) hr.col.index <- 3:4 else hr.col.index <- 3
    for (var.count in seq_along(var.names)) {
      if (!is.na(var.ref.groups[var.count])) {
        ref.group <- var.ref.groups[var.count]
        other.groups <- names(table(input.d[, var.names[var.count]]))
        other.groups <- other.groups[other.groups != ref.group & !(other.groups %in% missing.codes)]
        num.other.groups <- length(other.groups)
        result.table.bamboo.base.index <- result.table.bamboo.base.indexes[var.count]
        # for each survival end points e.g. os, dss, rfs
        for (i in seq_len(num.surv.endpoints)) {
          curr.base.index <- result.table.bamboo.base.index + (i - 1) * num.other.groups + 1
          if (num.other.groups > 1) {
            for (j in 1:(num.other.groups - 1)) {
              if (curr.base.index < nrow(result.table.bamboo)) {
                last.row.name <- rownames(result.table.bamboo)[nrow(result.table.bamboo)]
                result.table.bamboo <- rbind(
                  result.table.bamboo[1:curr.base.index, ],
                  rep("", ncol(result.table.bamboo)),
                  result.table.bamboo[(curr.base.index + 1):nrow(result.table.bamboo), ])
                rownames(result.table.bamboo)[nrow(result.table.bamboo)] <- last.row.name
              } else {
                result.table.bamboo <- rbind(
                  result.table.bamboo,
                  rep("", ncol(result.table.bamboo))
                )
              }
            }
          }
          if (num.other.groups > 1 | show.group.name.for.bin.var) {
            base_rows <- curr.base.index:(curr.base.index + num.other.groups - 1)
            result.table.bamboo[base_rows, hr.col.index] <-
              stringr::str_split_fixed(
                string = result.table.bamboo[curr.base.index, hr.col.index],
                pattern = kLocalConstantHrSepFlag,
                n = num.other.groups) %>%
              t()
            result.table.bamboo[base_rows, 2] <- other.groups
          }
        }

        # need to update result.table.bamboo.base.indexes since we've added rows!!!
        if (var.count < nvar) {
          result.table.bamboo.base.indexes[(var.count + 1):nvar] <- result.table.bamboo.base.indexes[(var.count + 1):nvar] +
            (num.other.groups - 1) * num.surv.endpoints
        }
      }
    }
    # if the column of hazard ratio category ends up being empty, remove it
    if (sum(result.table.bamboo[, hr.col.index - 1] == "") == nrow(result.table.bamboo)) {
      hr.col.index <- hr.col.index - 1
      result.table.bamboo <- result.table.bamboo[, -hr.col.index]
    }
  }

  # Reformat if "wide" format chosen
  format <- match.arg(format)
  if (format == "wide") {
    group_sizes <- diff(c(result.table.bamboo.base.indexes, nrow(result.table.bamboo) + 1))
    splits <- purrr::map2(result.table.bamboo.base.indexes, group_sizes, ~ seq(from = .x, length.out = .y))
    tab_splits <- purrr::map(splits, ~ result.table.bamboo[., ])

    result.table.bamboo <- purrr::map_dfr(tab_splits, ~ {
      data.frame(outcome = rownames(.x), .x, check.names = FALSE) %>%
        dplyr::rename_if(names(.) == "V2", ~ "Levels") %>%
        dplyr::mutate(Variable = .[["outcome"]][1]) %>%
        dplyr::relocate(Variable, .before = 1) %>%
        utils::tail(-1) %>%
        dplyr::mutate(across("outcome", ~ ifelse(. == "", NA, .))) %>%
        tidyr::fill(outcome) %>%
        tidyr::pivot_longer(
          cols = c("# of events / n", "Hazard Ratio (95% CI)", matches("P-value")),
          names_to = "stat",
          values_to = "values"
        ) %>%
        tidyr::unite(outcome.stat, c("outcome", "stat"), sep = ": ") %>%
        tidyr::pivot_wider(names_from = "outcome.stat", values_from = "values") %>%
        dplyr::mutate(Variable = ifelse(duplicated(.data$Variable), "", .data$Variable))
    }) %>%
      rlang::set_names(gsub(".*:.*(Hazard|P-value)", "\\1", names(.))) %>%
      as.matrix()
  }

  # subscript ("<sup>|</sup>") and line break ("<br>") syntax for pandoc
  options("table_counter" = options()$table_counter - 1)
  result.table.bamboo <- result.table.bamboo %>%
    gsub(pattern = "<sup>|</sup>", replacement = "^", .) %>%
    pander::pandoc.table.return(., caption = caption,
                                emphasize.rownames = FALSE,
                                split.table = split.table, ...) %>%
    gsub(pattern = kLocalConstantHrSepFlag, replacement = "; ", .) %>%
    gsub(pattern = "<br>", replacement = "\\\\\n", .)
  ### end of result.table.bamboo ###

  ### clean result.table ###
  result.table <- gsub(kLocalConstantHrSepFlag, ", ", result.table)
  ### end of clean result.table ###
  dplyr::lst(result.table, result.table.bamboo, result.table.html)
}
