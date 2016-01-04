dehistSum<-function(var,xlab="",txt="",sub=""){
  h<-hist(var,main=paste("Mean", round(mean(var,na.rm=T),2), "SD",round(sd(var,na.rm=T),2)),xlab=xlab,col="azure3",sub=sub,prob=F)
  xfit<-seq(min(var,na.rm=T),max(var,na.rm=T),length=50)
  yfit<-dnorm(xfit,mean=mean(var,na.rm=T),sd=sd(var,na.rm=T))
  yfit <- yfit*diff(h$mids[1:2])*length(var)
  lines(xfit, yfit, col="blue", lwd=2)


  mtext(txt, side=3, outer=TRUE, line=-3)
}

boxplotSum<-function(var,ttl){
  bxp <- boxplot(var, horizontal=TRUE, axes=FALSE, main=ttl)
  mtext(c("Min","Q1","Med","Q3","Max"), side=3, at=bxp$stats, line=-7)
  mtext(c(round(bxp$stats[1],2),round(bxp$stats[2],2),round(bxp$stats[3],2),round(bxp$stats[4],2),round(bxp$stats[5],2)), side=3, at=bxp$stats, line=-8)
}

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

