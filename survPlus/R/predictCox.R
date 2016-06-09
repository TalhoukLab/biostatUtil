#' A function to make prediction in cox model (works for both coxph and coxphf)
#'@export
predictCox<- function(mod, newdata){
  #https://stats.stackexchange.com/questions/44896/how-to-interpret-the-output-of-predict-coxph/44911#44911
    obj <- decomposeSurvform(mod$formula,newdata)
    rMean <- coef(mod)%*%mod$means
    rNew <- as.vector(coef(mod)%*%t(obj$mm1))
    pred <- rNew-rMean
    return(pred)
}
