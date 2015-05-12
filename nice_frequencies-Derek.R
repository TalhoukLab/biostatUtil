############################
# kappa with bootstrap CI
# by Derek Chiu (2015-05-07)
#
kappaCIboot<-function(x,y,seed=20,num.boot=1000,conf.level=0.95){
  require(boot)
  require(psy)
  set.seed(seed)

  # Function to compute cohen's kappa with binary data and `conf.level`% CI based on `num.boot` bootstrap samples
  ckappa.boot <- function(data,x) {ckappa(data[x,])[[2]]}
  res <- boot(cbind(x,y),ckappa.boot,num.boot)
  bootCI <- boot.ci(res,type = "bca",conf = conf.level)   # adjusted bootstrap percentile (BCa) confidence interval (better)
  kappa <- c(PointEst = bootCI[[2]], bootCI[[4]][4:5])
  names(kappa)[2:3] <- c(paste0((1-conf.level)/2*100, "%"), paste0((1-(1-conf.level)/2)*100, "%"))
  return(kappa)
}

############################
# confusion matrix with confidence interval for 2 categories
# by Derek Chiu (2015-05-07)
#
confusion.matrix <- function(x,y,seed=20,num.boot=1000,conf.level=0.95,digits=4){
  require(Hmisc)
  cat("Confusion Matrix", "\n")
  CM=table(Prediction=x,Reference=y);
  print(CM)
  
  # Compute marginal totals and correct predictions
  m1=CM[1,1]+CM[2,1]
  m2=CM[1,2]+CM[2,2]
  n1=CM[1,1]+CM[1,2]
  n2=CM[2,1]+CM[2,2]
  PO=CM[1,1]+CM[2,2]
  
  # Calculate statistics and obtain confidence intervals
  Accuracy=round(binconf(PO,m1+m2,alpha=1-conf.level,method="wilson"),digits)
  Sensitivity=round(binconf(CM[1,1],m1,alpha=1-conf.level,method="wilson"),digits)
  Specificity=round(binconf(CM[2,2],m2,alpha=1-conf.level,method="wilson"),digits)
  PPV=round(binconf(CM[1,1],n1,alpha=1-conf.level,method="wilson"),digits)
  NPV=round(binconf(CM[2,2],n2,alpha=1-conf.level,method="wilson"),digits)
  kappa=round(kappaCIboot(x,y,seed,num.boot,conf.level),digits)
  colnames(Sensitivity)[2:3] <- colnames(Specificity)[2:3] <- colnames(PPV)[2:3] <- colnames(NPV)[2:3] <- 
    c(paste0((1-conf.level)/2*100, "%"), paste0((1-(1-conf.level)/2)*100, "%"))
  
  # Print results
  printCI<-function(z){paste(z[1],"(",z[2],"-",z[3],")")}
  cat(paste0("\n", "Accuracy: ", printCI(Accuracy), "\n", "Sensitivity: ", printCI(Sensitivity), "\n",
             "Specificity: ", printCI(Specificity), "\n", "PPV: ", printCI(PPV), "\n",
             "NPV: ", printCI(NPV), "\n", "kappa: ", printCI(kappa), "\n", "\n"))
  
  return(list(Accuracy=Accuracy,Sensitivity=Sensitivity,Specificity=Specificity,PPV=PPV,NPV=NPV,kappa=kappa))
}

x <- rbinom(100, 1, 0.5)
y <- rbinom(100, 1, 0.2)
confusion.matrix(x, y, conf.level = 0.9)

#########################################################
# generate a table of row percentages given the table t
row.percent <- function(t, pretty.text=FALSE, digits=4) {
  if (pretty.text) {
    # e.g. 12%	
    return(
      apply(
        t/apply(t,1,sum)*100,
        c(1,2),
        function(x){
          if (!is.nan(x)) {
            return(paste0(format(x,digits=digits),"%"))
          } else {
            return("-")
          }
        }
      )
    )
  } 
  return(t/apply(t,1,sum))
}

row.percent.as.html <- function(
  t, 
  show.count=FALSE, # whether to show the count
  # pretty.text=FALSE, digits=4, # these are part of ... i.e. argment for row/col.percent
  row.names=NULL,
  column.names=NULL,
  html.table.border=0,
  banded.rows=FALSE,
  css.class.name.odd="odd",
  css.class.name.even="even",...
) {
  col.th.style <- "COL.TH.STYLE"
  row.th.style <- "ROW.TH.STYLE"
  table.values <- row.percent(t, pretty.text=pretty.text, digits=digits,...)
  result <- paste("<table border=",html.table.border,">",sep="")
  # print header
  if (!is.null(column.names)) {
    colnames(table.values) <- column.names
  }
  if (!is.null(row.names)) {
    rownames(table.values) <- row.names
  } else {
    row.names <- rownames(table.values)
  }
  result <- paste(result,"<tr><th style='",col.th.style,"'>",paste(names(table.values),collapse=paste("</th><th style='",col.th.style,"'>",sep="")),"</th></tr>",sep="")
  
  # print values
  if (length(dim(table.values))==1) {
    if (show.count) {
      result <- paste(result,"<tr><td>",paste(paste(t," (",table.values,")",sep=""),collapse="</td><td>"),"</td></tr>",sep="")
    } else {
      result <- paste(result,"<tr><td>",paste(table.values,collapse="</td><td>"),"</td></tr>",sep="")
    }
  } else {
    # for tables with > 1 row, there will be need to be row names
    for (i in 1:nrow(table.values)) {
      tr.class <- ifelse(banded.rows,paste(" class='",ifelse(i%%2==0,css.class.name.even,css.class.name.odd),"'",sep=""),"")
      result <- paste(result,"<tr",tr.class,"><th style='",row.th.style,"'>",row.names[i],"</th>",sep="")
      if (show.count) {
        result <- paste(result,"<td>",paste(paste(t[i,]," (",table.values[i,],")",sep=""),collapse="</td><td>"),"</td></tr>",sep="")	
      } else {
        result <- paste(result,"<td>",paste(table.values[i,],collapse="</td><td>"),"</td></tr>",sep="")
      }
    }
  }
  result <- paste(result,"</table>",sep="")
  return(result)
}

#########################################################
# generate a table of row percentages given a table with 
# a single row
# - i.e. percent over total
single.row.percent <- function(t,pretty.text=FALSE, digits=4) {
  row.percent <- t/sum(t)
  if (pretty.text) {
    row.percent <- sapply(
      t/sum(t)*100,
      function(x){
        paste(format(x,digits=digits),"%",sep="")
      },
      USE.NAMES=FALSE
    )
  } 
  return(rbind(t,row.percent))
}
