varianceFromCI<- function(HR,llimit,ulimit,alpha){
  #function to extract variance from a confidence limit and a corresponding pvalue, which can be compared to the reported pvalue
  # HR: reported hazard ratio
  # llimit: reported lower confidence limit
  # ulimit: reported upper confidence limit
  # alpha is a level of significance 
  # returns the variance and the estimated Pvalue
  varlnHR<-((log(ulimit)-log(llimit))/(2*qnorm(1-alpha/2)))^2
  chiTest<-(log(HR)/sqrt(varlnHR))^2
  estPval<-pchisq(chiTest,1,lower.tail=F)
return(list(var=varlnHR,Pval=estPval))
}

