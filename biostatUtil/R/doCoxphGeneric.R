#' coxph model generic
#' @export
doCoxphGeneric <- function(
  input.d, 
  var.names, 
  var.descriptions,
  show.var.detail=FALSE, # whether to show automatically generated details (i.e. categories and reference group) of variable - NOT DONE YET!!!!
  var.ref.groups = NULL, # a list of reference group, if NULL, assume ALL variables are binary/continuous, if individual item in array is NA, assume that particular marker is binary or continuous ... i.e. treat it as a numeric variable
  var.names.surv.time   = c("os.yrs",  "dss.yrs",  "rfs.yrs"  ), # variable names of survival time
  var.names.surv.status = c("os.sts",  "dss.sts",  "rfs.sts"  ), # variable names of survival status
  event.codes.surv      = c("os.event","dss.event","rfs.event"), # event coding of survival status variable
  surv.descriptions     = c("OS",      "DSS",      "RFS"      ), # description of survival endpoint
  missing.codes = c("N/A", "", "Unk"),
  use.firth = 1, # the percentage of censored cases before using the Firth method for Cox regression, 1 means NEVER use
  firth.caption = FIRTH.CAPTION, # a text in html table to indicate that Firth was used.
  stat.test = "waldtest", # can be "logtest", "waldtest", "sctest" ... if use Firth, can only do Likelihood ratio test
  round.digits.p.value = 4, # number of digits for p-value
  caption = NA, # caption for table
  html.table.border = 0,
  banded.rows = FALSE,
  css.class.name.odd = "odd",
  css.class.name.even = "even",
  split.table=200, # set default for pander
  ...) {
  kLocalConstantHrSepFlag <- "kLocalConstantHrSepFlag" # separates the hazard ratio estimates 
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  for (var.name in var.names) {
    if (is.factor(input.d[, var.name])) {
      input.d[, var.name] <- droplevels(input.d[, var.name])
    }
  }
  
  num.surv.endpoints <- length(var.names.surv.time)
  
  # qc ... make sure length of var.names.surv.time=var.names.surv.status=event.codes.surv=surv.descriptions
  if (length(var.names.surv.time) != length(var.names.surv.status) |
      length(var.names.surv.time) != length(event.codes.surv) |
      length(var.names.surv.time) != length(surv.descriptions)) {
    cat("ERROR IN do.coxph.generic!!!! input error ... please double check\n")
  }
  for (i in 1:num.surv.endpoints) {
    input.d[, var.names.surv.time[i]] <- as.numeric(
      input.d[, var.names.surv.time[i]])
  }
  
  if (is.null(var.ref.groups)) {
    var.ref.groups <- rep(NA, length(var.names))
  }
  result.table <- c()
  for (i in 1:length(var.names)) {
    var.name <- var.names[i]
    temp.d <- input.d[!is.na(input.d[, var.name]), ] # remove any cases with NA's for marker
    temp.d <- temp.d[!sapply(temp.d[, var.name], as.character)
                     %in% missing.codes, ]
    if (is.na(var.ref.groups[i])) {
      temp.d[,var.name] <- as.numeric(temp.d[, var.name]) # numeric
      var.levels <- c(0, 1) # dummy levels ... used to build result.table
    } else {
      # assume ref group exist!!!
      var.levels <- names(table(temp.d[, var.name]))
      var.levels <- c(var.ref.groups[i], var.levels[-which(
        var.levels == var.ref.groups[i])])
      temp.d[, var.name] <- factor(temp.d[, var.name], levels = var.levels)
    }
    
    for (j in 1:num.surv.endpoints) {
      surv.formula <- as.formula(paste0("Surv(", var.names.surv.time[j], ", ", var.names.surv.status[j], "=='", event.codes.surv[j], "'  ) ~", var.name))
      temp.d.no.missing.survival <- temp.d[!is.na(
        temp.d[,var.names.surv.status[j]]) &
          !is.na(temp.d[,var.names.surv.time[j]]), ]
      cox.stats  <- prettyCoxph(surv.formula, 
        input.d = temp.d.no.missing.survival,
        use.firth = use.firth)
      result.table <- rbind(
        result.table,
        "DUMMY_ROW_NAME" = c(
          # paste(cox.stats$nevent,cox.stats$n,sep="/"),
          paste(cox.stats$nevent, cox.stats$n, sep=" / "),
          paste(paste0(
            sprintf("%.2f", round(as.numeric(cox.stats$output[, 1]), 2)), " (",
            sprintf("%.2f", round(as.numeric(cox.stats$output[, 2]), 2)), "-",
            sprintf("%.2f", round(as.numeric(cox.stats$output[, 3]), 2)), ")",
            ifelse(cox.stats$used.firth, firth.caption, "")), collapse = kLocalConstantHrSepFlag),
          sprintf(
            paste0("%.", round.digits.p.value, "f"),
            round(
              # per instruction from Aline 2015-04-14,15 ... for p-value, ALWAYS use coxph
              as.numeric(summary(cox.stats$fit)[[stat.test]]["pvalue"]), # waldtest for Wald test, logtest for likelihood ratio test
              digits <- round.digits.p.value
            )
          ) 
        )
      )
    }
  }
  result.table.col.names <- c("# of events / n", "Hazard Ratio (95% CI)",
                              paste0(ifelse(stat.test == "logtest", "LRT ", ""),
                                    "P-value"))
  colnames(result.table) <- result.table.col.names
  result.table.row.names <- c()
  for (i in 1:length(var.names)) {
    result.table.row.names <- c(result.table.row.names, paste0(var.names[i], paste0("-", surv.descriptions)))
  }
  rownames(result.table) <- result.table.row.names
  
  ### generate html table ... ###
  result.table.html <- paste0("<table border=", html.table.border, ">",
                             ifelse(is.na(caption), "",
                                    paste0("<caption style='",
                                           TABLE.CAPTION.STYLE, "'>",
                                           addTableNumber(caption),
                                           "</caption>")))
  result.table.html <- paste0(result.table.html, "<tr><th style='",
                              col.th.style, "' colspan=2></th><th style='",
                              col.th.style, "'>",
                              paste(result.table.col.names,
                                    collapse = paste0("</th><th style='",
                                                      col.th.style, "'>")),
                              "</th></tr>")
  # print values
  i <- 1
  num.row.per.cat <- length(var.names.surv.time)
  while (i <= nrow(result.table) ) {
    is.first.row <- TRUE
    tr.class <- ifelse(banded.rows,
                       paste0(" class='",
                             ifelse((floor(i / num.surv.endpoints) + 1) %%
                                      2 == 0, css.class.name.even,
                                    css.class.name.odd), "'"), "")
    var.index <- floor((i - 1) / num.surv.endpoints) + 1
    var.description <- var.descriptions[var.index]
    if (show.var.detail) {
     var.categories <- names(table(input.d[,var.names[var.index]]))
     var.categories <- var.categories[!var.categories %in% missing.codes]
     if (!is.na(var.ref.groups[var.index])) {
       # this variable is categorical ... print ref group and categories
        var.description <- paste0(var.description,":<br><br><i>",paste(var.categories,collapse="<br>"),"<br><br>(reference group:",var.ref.groups[var.index],")</i>")
      }
    }
    result.table.html <- paste0(result.table.html, "<tr", tr.class,
                                "><th style='", row.th.style, "' rowspan=",
                                num.row.per.cat, ">",
                                var.description, "</th>")
    for (surv.description in surv.descriptions) {
      result.table.html <- paste0(
        result.table.html,
        ifelse(is.first.row, "", paste0("<tr", tr.class, ">")),
        "<th style='", row.th.style, "'>", surv.description, "</th><td>",
        paste(gsub(kLocalConstantHrSepFlag,"<br>",result.table[i, ]), collapse = "</td><td>"), "</td></tr>")
      
      is.first.row <- FALSE # if run any time after the first row, must not be the first row any more
      i <- i + 1
    }
  }
  result.table.html <- paste0(result.table.html, "</table>")
  ### end of generate html table ###
  
  ### generate word-friendly table via pander i.e. result.table.bamboo ... ###
  result.table.bamboo <- result.table
  # want to add empty rows for var description
  for (var.count in 1:length(var.names)) {  
    result.table.bamboo.base.index <- 1 + (var.count-1)*4
    result.table.ncol <- ncol(result.table)
    if (var.count==1) {
      result.table.bamboo <- rbind(rep("",result.table.ncol),result.table.bamboo)
    } else {
      result.table.bamboo <- rbind(
        result.table.bamboo[1:(result.table.bamboo.base.index-1),],
        rep("",result.table.ncol),
        result.table.bamboo[result.table.bamboo.base.index:nrow(result.table.bamboo),])
    }
    rownames(result.table.bamboo)[result.table.bamboo.base.index] <- paste("**",var.descriptions[var.count],"**",sep="")
    rownames(result.table.bamboo)[result.table.bamboo.base.index+c(1:3)] <- surv.descriptions
  }
  options("table_counter" = options()$table_counter - 1)
  result.table.bamboo <- pander::pandoc.table.return(
    result.table.bamboo, caption = paste0("*", addTableNumber(caption), "*"), emphasize.rownames = FALSE, split.table=split.table, ...)
  result.table.bamboo <- gsub(kLocalConstantHrSepFlag,"; ",result.table.bamboo)
  ### end of result.table.bamboo ###

  ### clean result.table ###
  result.table <- gsub(kLocalConstantHrSepFlag,", ",result.table)
  ### end of clean result.table ###
  
  return(list("result.table" = result.table,
          "result.table.bamboo" = result.table.bamboo,
              "result.table.html" = result.table.html))
}