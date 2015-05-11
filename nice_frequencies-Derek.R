############################
# kappa with bootstrap CI
# by Derek Chiu (2015-05-07)
#
kappaCIboot<-function(x,y,seed=20,num.boot=1000,conf.level=0.95){
  require(boot)
  require(psy)
  set.seed(seed)

  #Function to compute cohen's kappa with binary data and `conf.level`% CI based on `num.boot` bootstrap samples
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
