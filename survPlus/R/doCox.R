#' Perform Coxph or Coxphf
#'@export
doCox <- function(formula, data0, firth=TRUE){
  # Decompose the model
  obj <- decomposeSurvform(formula, data0)
  surv <- obj$survpair
  predictors <- obj$predictors
  complete.index <- obj$indcc
  data=data0[complete.index,]
  if (firth){
    fit <- coxphf::coxphf(formula,data)
    res <- cbind(beta.lp=fit$coefficients,lower= log(fit$ci.lower),upper= log(fit$ci.upper))
    loglik <- fit$loglik #the null and maximimized (penalized) log likelihood
    }else{
      fit <- survival::coxph(formula,data)
      res <- cbind(beta.lp=fit$coefficients,lower= log(summary(fit)$conf.int[,3]),upper= log(summary(fit)$conf.int[,4]))
    loglik <- fit$loglik
    }
  return(list(model=fit,results=res,loglik=loglik, firth=firth))
  }