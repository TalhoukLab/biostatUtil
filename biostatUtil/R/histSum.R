#'Summary histogram
#'
#'Function to create an annotated histogram, with density plot
#'@param var  a variable to be plotted
#'@param xlab X axis label
#'@param txt  text to be used in the title (defaults to none)
#'@param sub  a sublabel (defaults to none)
#'@param digit number of digits used in rounding (defaults to 1)
#'@author Aline Talhouk
#'@export
histSum<-function(var, xlab = "", txt = "", sub = "", digit = 1){
  h<-hist(var, main = paste("Mean", round(mean(var, na.rm = T), digit), "SD",round(sd(var,na.rm=T),digit), "Missing", sum(is.na(var))), prob = F, xlab = xlab, col= "white", border = "grey", sub = sub, cex = 0.8)
  xfit<-seq(min(var,na.rm=T),max(var,na.rm=T),length=50)
  yfit<-dnorm(xfit,mean=mean(var,na.rm=T),sd=sd(var,na.rm=T))
  yfit <- yfit*diff(h$mids[1:2])*length(var)
  lines(xfit, yfit, col="blue", lwd=2)
  mtext(txt, side=3, outer=TRUE, line=-3)
}
