

SumPlots<-function(df){
genes=colnames(df)
require(xtable)
res=NULL
for(counter in 1:ncol(df)){

  var=df[,counter]
  if(is.numeric(var)){
    # five point summary, histogram,boxplot, qqplot
    par(oma=c(0,0,5,0))
    layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE), heights=c(1.5,2))
    boxplotSum(var)
    histSum(var,xlab=genes[counter])
    qqnorm(var)
    qqline(var)
    title(colnames(df)[counter], outer=T)
  }else{
    xout <- as.data.frame(table(var))
    #Add cumFreq and proportions
    xout <- transform(xout, cumFreq = cumsum(Freq), relative = round(prop.table(Freq),3))
    res=rbind(res,xout)
  }
}
return(res)
}

