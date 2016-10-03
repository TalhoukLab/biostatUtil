test.bootMean<-function(){
  mySeed<-789
  set.seed(mySeed)

  ##Create test vector and matrix
  test.vec<-rnorm(n=60,mean=7,sd=3)
  test.mat<-matrix(test.vec,nrow=10)
  vecTestResult<-biostatUtil::bootMean(test.vec)
  matTestResult<-biostatUtil::bootMean(test.mat)

  ##Check the vector
  checkEquals(vecTestResult$n,length(test.vec))
  checkEquals(length(vecTestResult$obs.mean),1)
  checkEquals(signif(vecTestResult$obs.mean),signif(6.725579))
  checkEquals(signif(vecTestResult$ci[1]),signif(5.983460))
  checkEquals(signif(vecTestResult$ci[2]),signif(7.381294))

  ##Check the matrix
  checkEquals(matTestResult$n,length(test.mat))
  checkEquals(length(matTestResult$obs.mean),1)
  checkEquals(signif(matTestResult$obs.mean),signif(6.725579))
  checkEquals(signif(matTestResult$ci[1]),signif(5.983460))
  checkEquals(signif(matTestResult$ci[2]),signif(7.381294))

  #Check for cases with NA and na.rm=TRUE is in the argument
  set.seed(mySeed)
  s <- ifelse(rexp(100, 0.5) < 1, NA, rexp(100, 0.5))
  sTestResultNaRm<-bootMean(s,na.rm=TRUE)
  checkEquals(signif(sTestResultNaRm$obs.mean),signif(2.195333))
  checkEquals(signif(sTestResultNaRm$n),length(s))
  checkEquals(signif(sTestResultNaRm$ci[1]),signif(1.659419))
  checkEquals(signif(sTestResultNaRm$ci[2]),signif(2.715013))

  ##Check if character argument is passed not numeric
  checkException(bootMean("xx"))

  ##Check for cases with NA and na.rm is not in the argument
  checkException(bootMean(s),NA)

}
