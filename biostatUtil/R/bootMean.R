#' Bootstraped mean
bootMean<- function(x,num.boot=1000,random.seed=12,...){
  set.seed(random.seed)
  obs.mean <- mean(x,...)
  ci <- sort(sapply(1:num.boot,function(y){
    boot.x <- sample(x,replace=TRUE)
    return(mean(boot.x,...))
  },USE.NAMES=FALSE))[c(floor(num.boot*0.025),ceiling(num.boot*0.975))]
  return(list("obs.mean"=obs.mean,"ci"=ci,"n"=length(x)))
}