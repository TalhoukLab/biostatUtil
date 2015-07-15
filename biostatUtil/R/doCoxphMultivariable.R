#' coxph model multivariable
#' @export
doCoxphMultivariable <- function(
  input.d, 
  var.names, 
  var.descriptions,
  var.ref.groups=NULL, # a list of reference group, if NULL, assume ALL variables are binary/continuous, if individual item in array is NA, assume that particular marker is binary or continuous ... i.e. treat it as a numeric variable
  var.names.surv.time   = c("os.yrs",  "dss.yrs",  "rfs.yrs"  ), # variable names of survival time
  var.names.surv.status = c("os.sts",  "dss.sts",  "rfs.sts"  ), # variable names of survival status
  event.codes.surv      = c("os.event","dss.event","rfs.event"), # event coding of survival status variable
  surv.descriptions     = c("OS",      "DSS",      "RFS"      ), # description of survival endpoint
  missing.codes=c("N/A","","Unk"),
  use.firth=1, # the percentage of censored cases before using the Firth method for Cox regression, 1 means NEVER use
  firth.caption=FIRTH.CAPTION, # a text in html table to indicate that Firth was used.
  stat.test="waldtest", # can be "logtest", "waldtest" ... CANNOT DO "sctest" (log rank)... if use Firth, can only do Likelihood ratio test
  round.digits.p.value=4, # number of digits for p-value
  caption=NA, # caption for table
  html.table.border=0,
  banded.rows=FALSE,
  css.class.name.odd="odd",
  css.class.name.even="even",
  split.table=300, # set default for pander
  ...) {
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
  
  # qc ... make sure length of var.names.surv.time=var.names.surv.status=event.codes.surv=surv.descriptions
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
    cox.stats  <- prettyCoxph(
      full.model.formula, 
      input.d=temp.d,
      use.firth=use.firth,...
    )
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
               # use anova(cph())
               p.value <- anova(cph(as.formula(paste("Surv(",var.names.surv.time[j], ", ",var.names.surv.status[j], "=='",event.codes.surv[j], "'  ) ~",paste(var.names,collapse="+"),sep="")),temp.d))[i,"P"]
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
  
  ### generate html table ... ###
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

  ### generate word-friendly table via pander i.e. result.table.bamboo ... ###
  result.table.bamboo <- result.table
  result.table.ncol <- ncol(result.table)
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
  }
  ## subscript syntax for pandoc
  result.table.bamboo <- gsub(result.table.bamboo, pattern = "<sup>|</sup>", replacement = "^")
  
  options("table_counter" = options()$table_counter - 1)
  result.table.bamboo <- pander::pandoc.table.return(
      result.table.bamboo, caption = paste0("*", addTableNumber(caption), "*"), emphasize.rownames = FALSE, split.table=split.table, ...)
  result.table.bamboo <- gsub(kLocalConstantHrSepFlag,"; ",result.table.bamboo)
  ### end of result.table.bamboo ###  

  ### clean result.table ###
  result.table <- gsub(kLocalConstantHrSepFlag,", ",result.table)
  ### end of clean result.table ###
  
  return(list("result.table" = result.table,
              "result.table.html" = result.table.html,
              "result.table.bamboo" = result.table.bamboo,
              "cox.stats" = cox.stats.output))
}