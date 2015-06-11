#' coxphOut function
coxphOut <- function (x,coefnames=NULL,digits=2,use.firth=FALSE) {
  cox <- x # this can be either an object from coxph() or coxphf() ... slightly different output
  Coef <- cox$coef
  se <- sqrt(diag(cox$var))
  CI   <- exp(confint(cox))
  n <- cox$n #summary(cox)$n
  events <- ifelse(use.firth,sum(cox$y[,"status"]),cox$nevent) # events <- summary(cox)$nevent
  Z<- Coef/se
  p    <- 1 - pchisq((Coef/se)^2, 1)
  tmp <- cbind(n,events,Coef,se, Z,p, HR= exp(Coef),CI)
  if(is.null(coefnames)){coefnames=names(Coef)}
  dimnames(tmp) <- list(coefnames, c("n","events" ,"coef "," se "," Z-Score "," P-value "," HR ", " 2.5% ", " 97.5% "))
  return(round(tmp,digits))  
}