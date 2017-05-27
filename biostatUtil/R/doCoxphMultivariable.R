#' Fit a multivariable Cox model
#' 
#' Fits multivariable Cox models for each specified endpoints.
#' 
#' Please note the following assumptions. 1) Marker can be binary, continuous or
#' categorical. 2) Missing survival time/status variables are coded as \code{NA}
#' (i.e. will only be checked by \code{is.na()}). 3) Survival time/status
#' variable name specified in the following order: "os", "dss", "rfs". 4) Coding
#' of survival status is binary only (i.e. cannot take survival status of > 2
#' categories).
#' 
#' @param input.d The \code{data.frame} containing the data
#' @param var.names variables to include as univariable predictors
#' @param var.descriptions vector of strings to describe the variables as they
#'   are to appear in the table
#' @param show.var.detail logical. If \code{TRUE}, details such as categories
#'   and the reference group for categorical variables are shown.
#' @param show.group.name.for.bin.var logical. If \code{TRUE}, the non-reference
#'   group name is shown beside the hazard ratio for dichotomized variables.
#' @param var.ref.groups a vector of reference groups. If \code{NULL}, assume
#'   all variables are binary/continuous. If an item in the vector is \code{NA},
#'   assume that particular marker is binary or continuous (i.e., treat it as a
#'   numeric variable)
#' @param var.names.surv.time variable names of survival time
#' @param var.names.surv.status variable names of survival status
#' @param event.codes.surv event coding of survival status variable
#' @param surv.descriptions names abbreviated survival endpoints in returned
#'   output
#' @param missing.codes character strings of missing values used in
#'   \code{input.d}
#' @param use.firth percentage of censored cases before using Firth's method for
#'   Cox regression. If \code{use.firth = 1} (default), Firth is never used and
#'   if \code{use.firth = -1} Firth is always used.
#' @param firth.caption subscript in html table output indicating Firth was used
#' @param stat.test the overall model test to perform on the Cox regression
#'   model. Can be any of "waldtest", "logtest", or "sctest". If Firth is used,
#'   only "logtest" can be performed.
#' @param round.digits.p.value number of digits for p-value
#' @param round.small if \code{TRUE}, uses small number rounding via
#'   \code{round_small}
#' @param scientific if \code{TRUE}, uses scientific notation when rounding
#' @param caption caption for returned object
#' @param html.table.border the border type to use for html tables
#' @param banded.rows logical. If \code{TRUE}, rows have alternating shading
#'   colour
#' @param css.class.name.odd Used to set the row colour for odd rows
#' @param css.class.name.even Used to set the row colour for even rows
#' @param split.table number of characters per row before splitting the table.
#'   Applies to the pandoc table output.
#' @param ... additional arguments to \code{pandoc.table.return}
#' @return A list with the following elements
#' @author Samuel Leung, Aline Talhouk, Derek Chiu
#' @export
doCoxphMultivariable <- function(
  input.d, var.names, var.descriptions, show.var.detail = FALSE,
  show.group.name.for.bin.var = FALSE, var.ref.groups = NULL,
  var.names.surv.time = c("os.yrs", "dss.yrs", "rfs.yrs"),
  var.names.surv.status = c("os.sts", "dss.sts", "rfs.sts"),
  event.codes.surv = c("os.event", "dss.event", "rfs.event"),
  surv.descriptions = c("OS", "DSS", "PFS"),
  missing.codes = c("N/A", "", "Unk"),
  use.firth = 1, firth.caption = FIRTH.CAPTION,
  stat.test = "waldtest", round.digits.p.value = 4,
  round.small = FALSE, scientific = FALSE,
  caption = NA, html.table.border = 0, banded.rows = FALSE,
  css.class.name.odd = "odd", css.class.name.even = "even",
  split.table = 300, ...) {
  
  # Constants
  kLocalConstantHrSepFlag <- "kLocalConstantHrSepFlag" # separates HR estimates
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  row.td.style.for.multi.cox <- ROW.TD.STYLE.FOR.MULTI.COX
  row.td.style.for.multi.cox.align.top <- ROW.TD.STYLE.FOR.MULTI.COX.ALIGN.TOP
  
  # Initial assertion checks
  num.surv.endpoints <- length(var.names.surv.time)
  assertthat::assert_that(num.surv.endpoints == length(var.names.surv.status),
                          num.surv.endpoints == length(event.codes.surv),
                          num.surv.endpoints == length(surv.descriptions))
  
  # Remove all variables not used in analysis, ensure survival times are numeric
  input.d <- input.d %>% 
    dplyr::select(dplyr::one_of(c(var.names, var.names.surv.time,
                                  var.names.surv.status))) %>% 
    droplevels() %>% 
    dplyr::mutate_at(var.names.surv.time, as.numeric)
  
  # Setup default for variable reference groups and result matrix
  nvar <- length(var.names)
  var.ref.groups <- var.ref.groups %||% rep(NA, nvar)
  rn <- paste(var.names, rep(surv.descriptions, each = nvar), sep = "-")
  cn <- c("# of events / n", "Hazard Ratio (95% CI)",
          paste0(ifelse(stat.test == "logtest", "LRT ", ""), "P-value"))
  result.table <- matrix(NA_character_, nrow = nvar * num.surv.endpoints,
                         ncol = 3, dimnames = list(rn, cn))
  for (i in seq_along(var.names)) {
    x <- var.names[i]
    input.d <- input.d %>%  # remove any cases with NA's or missing values
      dplyr::filter(!is.na(.[, x]) & !(.[, x] %in% missing.codes))
    if (is.factor(input.d[, x]) & is.na(var.ref.groups[i])) {  # automatically set ref.group to lowest group if not specified
      var.ref.groups[i] <- names(table(input.d[, x]))[1]
    }
    if (is.na(var.ref.groups[i])) {
      input.d[, x] <- as.numeric(input.d[, x])
    } else {
      input.d[, x] <- relevel(as.factor(input.d[, x]), var.ref.groups[i])
    }
  }
  
  cox.stats.output <- list()
  for (j in seq_len(num.surv.endpoints)) {
    surv.formula <- surv_formula(var.names.surv.time[j],
                                 var.names.surv.status[j], event.codes.surv[j],
                                 var.names)
    temp.d <- input.d %>% 
      dplyr::filter(!is.na(.[, var.names.surv.status[[j]]]) &
                      !is.na(.[, var.names.surv.time[[j]]]))
    cox.stats <- prettyCoxph(surv.formula, input.d = temp.d,
                             use.firth = use.firth)
    var.idx <- 0
    for (i in seq_along(var.names)) {
      var.name <- var.names[i]
      var.idx <- max(var.idx) + 1
      if (!is.na(var.ref.groups[i]))
        var.idx <- var.idx:(var.idx + dplyr::n_distinct(temp.d[, var.name]) - 2)
      e.n <- paste(cox.stats$nevent, "/", cox.stats$n)
      hr.ci <- cox.stats$output %>% 
        magrittr::extract(var.idx, c("estimate", "conf.low", "conf.high")) %>% 
        format_hr_ci(digits = 2, labels = FALSE, method = "Sci") %>% 
        paste0(ifelse(cox.stats$used.firth, firth.caption, "")) %>% 
        paste(collapse = kLocalConstantHrSepFlag)
      p.value <- switch(
        stat.test, 
        logtest = {
          cox.exclude.var <- coxph(surv_formula(var.names.surv.time[j],
                                                var.names.surv.status[j],
                                                event.codes.surv[j],
                                                var.names[-i]), temp.d)
          anova(cox.stats$fit, cox.exclude.var)[["P(>|Chi|)"]][2]
        },
        waldtest = {
          anova(rms::cph(surv_formula(var.names.surv.time[j],
                                      var.names.surv.status[j],
                                      event.codes.surv[j],
                                      var.names), temp.d))[i, "P"]
        }
      ) %>% 
        round_pval(round.small = round.small, scientific = scientific,
                   digits = round.digits.p.value)
      result.table[num.surv.endpoints * (j - 1) + i, ] <- c(e.n, hr.ci, p.value)
      cox.stats.output[[surv.descriptions[j]]] <- cox.stats
    }
  }
  
  ### generate html table ###
  result.table.html <- paste0("<table border=", html.table.border, ">",
                              ifelse(is.na(caption), "", paste0("<caption style='", TABLE.CAPTION.STYLE, "'>", caption, "</caption>")),
                              "<tr><th style='", col.th.style, "' colspan=2></th><th style='", col.th.style, "'>",
                              paste(colnames(result.table), collapse = paste0("</th><th style='", col.th.style, "'>")), "</th></tr>")
  # print values
  i <- 1
  nvar <- length(var.names)
  while (i <= nrow(result.table)) {
    is.first.row <- TRUE
    tr.class <- ifelse(banded.rows, paste0(" class='", ifelse((floor(i / nvar) + 1) %% 2 == 0,
                                                             css.class.name.even, css.class.name.odd), "'"), "")
    result.table.html <- paste0(result.table.html, "<tr", tr.class, "><th style='", row.td.style.for.multi.cox.align.top, "' rowspan=",
                                nvar, ">", surv.descriptions[floor((i - 1) / nvar) + 1], "</th>")
    for (j in seq_along(var.names)) {
      result.table.html <- paste0(
        result.table.html,
        ifelse(is.first.row, "", paste0("<tr", tr.class, ">")),
        "<th style='", row.td.style.for.multi.cox, "'>",
        var.descriptions[j],
        #"</th><td style='",row.td.style.for.multi.cox,"'>",paste(result.table[i,],collapse=paste("</td><td style='",row.td.style.for.multi.cox,"'>"),sep=""),"</td></tr>",
        ifelse(is.first.row,
               paste0("</th><td style='", row.td.style.for.multi.cox.align.top,
                      "' rowspan=", nvar, ">",
                      result.table[i, 1]),
               ""),
        "</td><td style='", row.td.style.for.multi.cox, "'>",
        paste0(gsub(kLocalConstantHrSepFlag, "<br>", result.table[i, 2:3]),
               collapse = paste("</td><td style='", row.td.style.for.multi.cox, "'>")), "</td></tr>")
      is.first.row <- FALSE # if run any time after the first row, must not be the first row any more
      i <- i + 1
    }
  }
  result.table.html <- paste0(result.table.html, "</table>")
  ### end of generate html table ###

  ### generate word-friendly table via pander i.e. result.table.bamboo ###
  result.table.bamboo <- result.table
  result.table.ncol <- ncol(result.table)
  result.table.bamboo.base.indexes <- c() # base indexes for each survival end point in result.table.bamboo
  num.surv <- length(surv.descriptions) # number of survival end points
  num.var <- length(var.descriptions) # number of variables
  for (i in seq_len(num.surv)) {
    result.table.bamboo.base.index <- 1 + (i - 1) * (num.var + 1)
    if (i == 1) {
      result.table.bamboo <- rbind(rep("", result.table.ncol), result.table.bamboo)
    } else {
      result.table.bamboo <- rbind(
          result.table.bamboo[1:(result.table.bamboo.base.index - 1), ],
          rep("", result.table.ncol),
          result.table.bamboo[result.table.bamboo.base.index:nrow(result.table.bamboo), ])
    }
    rownames(result.table.bamboo)[result.table.bamboo.base.index] <- paste0("**", surv.descriptions[i], "**")
    rownames(result.table.bamboo)[result.table.bamboo.base.index + c(1:num.var)] <- paste0(var.descriptions,
      ifelse(
        rep(show.var.detail, length(var.ref.groups)) * !is.na(var.ref.groups), # do not show ref group if no ref group to show
        paste0(" (reference group: ", var.ref.groups, ")"), ""))
    # want to show # of events only once for each surv endpoint
    result.table.bamboo[result.table.bamboo.base.index, 1] <- result.table.bamboo[result.table.bamboo.base.index + 1, 1]
    result.table.bamboo[result.table.bamboo.base.index + c(1:num.var), 1] <- ""
    result.table.bamboo.base.indexes <- c(result.table.bamboo.base.indexes, result.table.bamboo.base.index)
  }
  # want to add a column to describe different factor level for categorical 
  # whenever reference group is specified
  if (sum(is.na(var.ref.groups)) != length(var.ref.groups)) {
    first.col.name <- colnames(result.table.bamboo)[1]
    result.table.bamboo <- cbind(result.table.bamboo[, 1], "", result.table.bamboo[, 2:3])
    colnames(result.table.bamboo)[1] <- first.col.name
    hr.col.index <- 3 # column with the hazard ratios
    for (i in seq_len(num.surv)) {
      result.table.bamboo.base.index <- result.table.bamboo.base.indexes[i]
      rows.added <- 0
      for (var.count in seq_along(var.names)) {  
        if (!is.na(var.ref.groups[var.count])) {
          ref.group <- var.ref.groups[var.count]
          other.groups <- names(table(input.d[, var.names[var.count]]))
          other.groups <- other.groups[other.groups != ref.group & !(other.groups %in% missing.codes)]
          num.other.groups <- length(other.groups)
          curr.base.index <- result.table.bamboo.base.index + (var.count - 1) + rows.added + 1
          if (num.other.groups > 1) {
            for (j in seq_len(num.other.groups - 1)) {
              if (curr.base.index < nrow(result.table.bamboo)) {
                last.row.name <- rownames(result.table.bamboo)[nrow(result.table.bamboo)]
                result.table.bamboo <- rbind(
                    result.table.bamboo[1:curr.base.index, ],
                    rep("", ncol(result.table.bamboo)),
                    result.table.bamboo[(curr.base.index + 1):nrow(result.table.bamboo), ])
                rownames(result.table.bamboo)[nrow(result.table.bamboo)] <- last.row.name
              } else {
                result.table.bamboo <- rbind(
                    result.table.bamboo[1:curr.base.index, ],
                    rep("", ncol(result.table.bamboo))
                )
              }
              rows.added <- rows.added + 1
            }
          }
          if (num.other.groups > 1 | show.group.name.for.bin.var) {
            result.table.bamboo[curr.base.index:(curr.base.index + num.other.groups - 1), hr.col.index] <- 
              strsplit(result.table.bamboo[curr.base.index,hr.col.index], kLocalConstantHrSepFlag)[[1]]
            result.table.bamboo[curr.base.index:(curr.base.index + num.other.groups - 1), hr.col.index - 1] <- other.groups
          }
        }
      }
      
      # need to update result.table.bamboo.base.indexes since we've added rows!!!
      if (i < num.surv) {
        result.table.bamboo.base.indexes[(i + 1):num.surv] <- result.table.bamboo.base.indexes[(i + 1):num.surv] + rows.added
      }
    }
  }
  ## subscript syntax for pandoc
  result.table.bamboo <- gsub(result.table.bamboo, pattern = "<sup>|</sup>", replacement = "^")
  options("table_counter" = options()$table_counter - 1)
  result.table.bamboo <- pander::pandoc.table.return(
      result.table.bamboo, caption = caption,
      emphasize.rownames = FALSE, split.table = split.table, ...)
  result.table.bamboo <- gsub(kLocalConstantHrSepFlag, "; ", result.table.bamboo)
  
  ## line break syntax for pandoc
  result.table.bamboo <- gsub(x = result.table.bamboo, pattern = "<br>", replacement = "\\\\\n")
  ### end of result.table.bamboo ###  

  ### clean result.table ###
  result.table <- gsub(kLocalConstantHrSepFlag, ", ", result.table)
  ### end of clean result.table ###
  
  return(list("result.table" = result.table,
              "result.table.html" = result.table.html,
              "result.table.bamboo" = result.table.bamboo,
              "cox.stats" = cox.stats.output))
}
