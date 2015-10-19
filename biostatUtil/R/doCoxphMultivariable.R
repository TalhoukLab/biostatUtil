#' Fit a multivariable Cox model
#' 
#' Fits multivariable Cox models for each specified endpoints.
#' 
#' Please note the following assumptions. 1) Marker can be
#' binary, continuous or categorical. 2) Missing survival time/status variables
#' are coded as \code{NA} (i.e. will only be checked by \code{is.na()}).
#' 3) Survival time/status variable name specified in the following order: "os", "dss", "rfs".
#' 4) Coding of survival status is binary only (i.e. cannot take survival status of > 2 categories).
#' 
#' @param input.d The \code{data.frame} containing the data
#' @param var.names variables to include as univariable predictors
#' @param var.descriptions vector of strings to describe the variables as they are to appear in the table
#' @param show.var.detail logical. If \code{TRUE}, details such as categories and the reference group for
#' categorical variables are shown.
#' @param show.group.name.for.bin.var logical. If \code{TRUE}, the non-reference group name is shown beside
#' the hazard ratio for dichotomized variables.
#' @param var.ref.groups a vector of reference groups. If \code{NULL}, assume all variables are binary/continuous.
#' If an item in the vector is \code{NA}, assume that particular marker is binary or continuous
#' (i.e., treat it as a numeric variable)
#' @param var.names.surv.time variable names of survival time
#' @param var.names.surv.status variable names of survival status
#' @param event.codes.surv event coding of survival status variable
#' @param surv.descriptions names abbreviated survival endpoints in returned output
#' @param missing.codes character strings of missing values used in \code{input.d}
#' @param use.firth percentage of censored cases before using Firth's method for Cox regression.
#' If \code{use.firth = 1} (default), Firth is never used and if \code{use.firth = -1} Firth is
#' always used.
#' @param firth.caption subscript in html table output indicating Firth was used
#' @param stat.test the overall model test to perform on the Cox regression model. Can be any of
#' "waldtest", "logtest", or "sctest". If Firth is used, only "logtest" can be performed.
#' @param round.digits.p.value number of digits for p-value
#' @param caption caption for returned object
#' @param html.table.border the border type to use for html tables
#' @param banded.rows logical. If \code{TRUE}, rows have alternating shading colour
#' @param css.class.name.odd Used to set the row colour for odd rows
#' @param css.class.name.even Used to set the row colour for even rows
#' @param split.table number of characters per row before splitting the table. Applies to
#' the pandoc table output.
#' @param ... additional arguments to \code{pandoc.table.return}
#' @return A list with the following elements
#' @author Samuel Leung, Aline Talhouk, Derek Chiu
#' @import survival
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
  caption = NA, html.table.border = 0, banded.rows = FALSE,
  css.class.name.odd = "odd", css.class.name.even = "even",
  split.table = 300, ...) {
  
  kLocalConstantHrSepFlag <- "kLocalConstantHrSepFlag" # separates the hazard ratio estimates
  col.th.style <- COL.TH.STYLE
  row.th.style <- ROW.TH.STYLE
  row.td.style.for.multi.cox <- ROW.TD.STYLE.FOR.MULTI.COX
  row.td.style.for.multi.cox.align.top <- ROW.TD.STYLE.FOR.MULTI.COX.ALIGN.TOP
  for (var.name in var.names) {
    if (is.factor(input.d[,var.name])) {
      input.d[,var.name] <- droplevels(input.d[,var.name])
    }
  }
  
  num.surv.endpoints <- length(var.names.surv.time)
  
  # qc: make sure length of var.names.surv.time=var.names.surv.status=event.codes.surv=surv.descriptions
  if (length(var.names.surv.time) != length(var.names.surv.status) | 
      length(var.names.surv.time) != length(event.codes.surv) |
      length(var.names.surv.time) != length(surv.descriptions)) {
    cat("ERROR IN do.coxph.generic!!!! input error ... please double check\n")
  }
  for (i in 1:num.surv.endpoints) {
    input.d[,var.names.surv.time[i]] <- as.numeric(input.d[,var.names.surv.time[i]])
  }
  
  if (is.null(var.ref.groups)) {
    var.ref.groups <- rep(NA,length(var.names))
  }
  result.table <- c()
  for (i in 1:length(var.names)) {
    var.name <- var.names[i]
    input.d <- input.d[!sapply(input.d[,var.name],as.character) %in% missing.codes,]
    input.d <- input.d[!is.na(input.d[,var.name]),]
    # automatically set ref.group to lowest group if not specified
    if (is.factor(input.d[,var.name])) {
      input.d[, var.name] <- droplevels(input.d[, var.name])
      if (is.na(var.ref.groups[i])) {
        var.ref.groups[i] <- names(table(input.d[,var.name]))[1]
      }
    }
    if (is.na(var.ref.groups[i])) {
      input.d[,var.name] <- as.numeric(input.d[,var.name]) # numeric
      var.levels <- c(0,1) # dummy levels ... used to build result.table
    } else {
      # assume ref group exist!!!
      var.levels <- names(table(input.d[,var.name]))
      var.levels <- c(var.ref.groups[i],var.levels[-which(var.levels==var.ref.groups[i])])
      input.d[,var.name] <- factor(input.d[,var.name],levels=var.levels)
    }
  }
  
  cox.stats.output <- list()
  for (j in 1:num.surv.endpoints) {
    temp.d <- input.d[!is.na(input.d[,var.names.surv.status[j]]) & !is.na(input.d[,var.names.surv.time[j]]),]
    full.model.formula <- as.formula(paste("Surv(",var.names.surv.time[j], ", ",var.names.surv.status[j], "=='",event.codes.surv[j], "'  ) ~",paste(var.names,collapse="+"),sep=""))
    cox.stats  <- prettyCoxph(full.model.formula, input.d = temp.d,
                              use.firth = use.firth)
    cox.stats.output.indexes <- c(0)
    for (i in 1:length(var.names)) {
      var.name <- var.names[i]
      cox.stats.output.indexes <- max(cox.stats.output.indexes)+1
      if (!is.na(var.ref.groups[i])) {
        cox.stats.output.indexes <- c(cox.stats.output.indexes:(cox.stats.output.indexes+length(names(table(temp.d[,var.name])))-1-1))
      }
      
      p.value <- NA
      switch(stat.test, 
             logtest={
               # calculate likelihood ratio test by nested model
               cox.exclude.var <- coxph(as.formula(paste("Surv(",var.names.surv.time[j], ", ",var.names.surv.status[j], "=='",event.codes.surv[j], "'  ) ~",paste(var.names[-i],collapse="+"),sep="")),temp.d)	
               p.value <- anova(cox.stats$fit,cox.exclude.var)[[4]][2] # always the second one, since its comparing only two nested model
             },
             waldtest={
               p.value <- anova(rms::cph(as.formula(paste("Surv(",var.names.surv.time[j], ", ",var.names.surv.status[j], "=='",event.codes.surv[j], "'  ) ~",paste(var.names,collapse="+"),sep="")),temp.d))[i,"P"]
             }
      )	
      
      result.table <- rbind(
        result.table,
        "DUMMY_ROW_NAME" =c(
          paste(cox.stats$nevent,"/",cox.stats$n),
          paste(paste(
            sprintf("%.2f",round(as.numeric(cox.stats$output[cox.stats.output.indexes,1]),2))," (",
            sprintf("%.2f",round(as.numeric(cox.stats$output[cox.stats.output.indexes,2]),2)),"-",
            sprintf("%.2f",round(as.numeric(cox.stats$output[cox.stats.output.indexes,3]),2)),")",
            ifelse(cox.stats$used.firth,firth.caption,""),
            sep=""
          ),collapse=kLocalConstantHrSepFlag),
          sprintf(paste("%.",round.digits.p.value,"f",sep=""),round(as.numeric(p.value),digits=round.digits.p.value))) # waldtest for Wald test, logtest for likelihood ratio test
      )
      
      cox.stats.output[[surv.descriptions[j]]] <- cox.stats
    }
  }
  
  result.table.col.names <- c("# of events / n","Hazard Ratio (95% CI)",paste(ifelse(stat.test=="logtest","LRT ",""),"P-value",sep=""))
  colnames(result.table) <- result.table.col.names
  result.table.row.names <- c()
  for (i in 1:num.surv.endpoints) {
    result.table.row.names <- c(result.table.row.names,paste(var.names,paste("-",surv.descriptions[i],sep=""),sep=""))
  }
  rownames(result.table) <- result.table.row.names
  
  ### generate html table ###
  result.table.html <- paste("<table border=",html.table.border,">",ifelse(is.na(caption),"",paste("<caption style='",TABLE.CAPTION.STYLE,"'>",addTableNumber(caption),"</caption>",sep="")),sep="")
  result.table.html <- paste(result.table.html,"<tr><th style='",col.th.style,"' colspan=2></th><th style='",col.th.style,"'>",paste(result.table.col.names,collapse=paste("</th><th style='",col.th.style,"'>",sep="")),"</th></tr>",sep="")
  # print values
  i <- 1
  num.row.per.surv.type <- length(var.names)
  while(i <= nrow(result.table)) {
    is.first.row <- TRUE
    tr.class <- ifelse(banded.rows,paste(" class='",ifelse((floor(i/num.row.per.surv.type) + 1) %% 2 == 0, css.class.name.even,       css.class.name.odd),"'",sep=""),"")
    result.table.html <- paste(result.table.html,"<tr",tr.class,"><th style='",row.td.style.for.multi.cox.align.top,"' rowspan=",num.row.per.surv.type,">",surv.descriptions[floor((i-1)/num.row.per.surv.type)+1],"</th>",sep="")
    for (j in 1:length(var.names)) {
      result.table.html <- paste(
        result.table.html,
        ifelse(is.first.row,"",paste("<tr",tr.class,">",sep="")),
        "<th style='",row.td.style.for.multi.cox,"'>",
        var.descriptions[j],
        #"</th><td style='",row.td.style.for.multi.cox,"'>",paste(result.table[i,],collapse=paste("</td><td style='",row.td.style.for.multi.cox,"'>"),sep=""),"</td></tr>",
        ifelse(
          is.first.row,
          paste(
            "</th><td style='",row.td.style.for.multi.cox.align.top,"' rowspan=",num.row.per.surv.type,">",
            result.table[i,1],
            sep=""
          ),
          ""
        ),
        "</td><td style='",row.td.style.for.multi.cox,"'>",paste(gsub(kLocalConstantHrSepFlag,"<br>",result.table[i,2:3]),collapse=paste("</td><td style='",row.td.style.for.multi.cox,"'>"),sep=""),"</td></tr>",
        sep=""
      )
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
  for (i in 1:num.surv) {
    result.table.bamboo.base.index <- 1 + (i-1)*(num.var+1)
    if (i==1) {
      result.table.bamboo <- rbind(rep("",result.table.ncol),result.table.bamboo)
    } else {
      result.table.bamboo <- rbind(
          result.table.bamboo[1:(result.table.bamboo.base.index-1),],
          rep("",result.table.ncol),
          result.table.bamboo[result.table.bamboo.base.index:nrow(result.table.bamboo),])
    }
    rownames(result.table.bamboo)[result.table.bamboo.base.index] <- paste("**",surv.descriptions[i],"**",sep="")
    rownames(result.table.bamboo)[result.table.bamboo.base.index+c(1:num.var)] <- var.descriptions
    # want to show # of events only once for each surv endpoint
    result.table.bamboo[result.table.bamboo.base.index,1] <- result.table.bamboo[result.table.bamboo.base.index+1,1]
    result.table.bamboo[result.table.bamboo.base.index+c(1:num.var),1] <- ""
    result.table.bamboo.base.indexes <- c(result.table.bamboo.base.indexes,result.table.bamboo.base.index)
  }
  # want to add a column to describe different factor level for categorical 
  # whenever reference group is specified
  if (sum(is.na(var.ref.groups))!=length(var.ref.groups)) {
    first.col.name <- colnames(result.table.bamboo)[1]
    result.table.bamboo <- cbind(result.table.bamboo[,1],"",result.table.bamboo[,2:3])
    colnames(result.table.bamboo)[1] <- first.col.name
    hr.col.index <- 3 # column with the hazard ratios
    for (i in 1:num.surv) {
      result.table.bamboo.base.index <- result.table.bamboo.base.indexes[i]
      rows.added <- 0
      for (var.count in 1:length(var.names)) {  
        if (!is.na(var.ref.groups[var.count])) {
          ref.group <- var.ref.groups[var.count]
          other.groups <- names(table(input.d[,var.names[var.count]]))
          other.groups <- other.groups[other.groups!=ref.group&!(other.groups%in%missing.codes)]
          num.other.groups <- length(other.groups)
          
          curr.base.index <- result.table.bamboo.base.index + (var.count-1) + rows.added + 1
          if (num.other.groups>1) {
            for (j in 1:(num.other.groups-1)) {
              if (curr.base.index<nrow(result.table.bamboo)) {
                last.row.name <- rownames(result.table.bamboo)[nrow(result.table.bamboo)]
                result.table.bamboo <- rbind(
                    result.table.bamboo[1:curr.base.index,],
                    rep("",ncol(result.table.bamboo)),
                    result.table.bamboo[(curr.base.index+1):nrow(result.table.bamboo),])
                rownames(result.table.bamboo)[nrow(result.table.bamboo)] <- last.row.name
              } else {
                result.table.bamboo <- rbind(
                    result.table.bamboo[1:curr.base.index,],
                    rep("",ncol(result.table.bamboo))
                )
              }
              rows.added <- rows.added + 1
            }
          }
          if (num.other.groups>1 | show.group.name.for.bin.var) {
            result.table.bamboo[curr.base.index:(curr.base.index+num.other.groups-1),hr.col.index] <- 
              strsplit(result.table.bamboo[curr.base.index,hr.col.index],kLocalConstantHrSepFlag)[[1]]
            result.table.bamboo[curr.base.index:(curr.base.index+num.other.groups-1),hr.col.index-1] <- other.groups
          }
        }
      }
      
      # need to update result.table.bamboo.base.indexes since we've added rows!!!
      if (i<num.surv) {
        result.table.bamboo.base.indexes[(i+1):num.surv] <- result.table.bamboo.base.indexes[(i+1):num.surv] + rows.added
      }
    }
  }
  ## subscript syntax for pandoc
  result.table.bamboo <- gsub(result.table.bamboo, pattern = "<sup>|</sup>", replacement = "^")
  
  options("table_counter" = options()$table_counter - 1)
  result.table.bamboo <- pander::pandoc.table.return(
      result.table.bamboo, caption = paste0("*", addTableNumber(caption), "*"),
      emphasize.rownames = FALSE, split.table = split.table, ...)
  result.table.bamboo <- gsub(kLocalConstantHrSepFlag,"; ",result.table.bamboo)
  
  ## line break syntax for pandoc
  result.table.bamboo <- gsub(x = result.table.bamboo, pattern = "<br>", replacement = "\\\\\n")
  ### end of result.table.bamboo ###  

  ### clean result.table ###
  result.table <- gsub(kLocalConstantHrSepFlag,", ",result.table)
  ### end of clean result.table ###
  
  return(list("result.table" = result.table,
              "result.table.html" = result.table.html,
              "result.table.bamboo" = result.table.bamboo,
              "cox.stats" = cox.stats.output))
}
