#' A function to perform Cox proportional hazard with or without firth correction
#' @param formula a formula object, with the response on the left of a ~ operator, and the terms on the right. The response must be a survival object as returned by the Surv function.
#' @param data a data.frame in which to interpret the variables named in the formula.
#' @return
#' \item{model} n object of class coxph representing the fit. See coxph.object for details.
#' \item{formula} formula used
#' \item {predtable} predictor table on the log scale with upper and lower bounds
#' \item{loglik} the loglikelihood
#' \item{firth} whether a firth correction was performed.
#' @author Aline Talhouk
#' @export
#' @examples
#' dat <- data.frame(time=c(4,3,1,1,2,2,3),
#' status=c(1,1,1,0,1,1,0),
#' x=c(0,2,1,1,1,0,0),
#' sex=c(0,0,0,0,1,1,1))
#' doCox(Surv(time, status) ~ x + strata(sex), data=dat)

doCox <- function(formula, data, firth=TRUE){
  if(!is.data.frame(data)){stop("Must provide data in a data.frame")}
  # Decompose the model
  obj <- decomposeSurvform(formula, data)
  surv <- obj$survpair #identifies time and status (response variable)
  predictors <- obj$predictors #identifies the predictors
  complete.index <- obj$indcc #identifies complete cases
  datac <- data[complete.index,] #Use only complete cases

  #Do if firth correction is requested (default)
  if (firth){
    fit <- coxphf::coxphf(formula,data)
    res <- cbind(beta.lp=fit$coefficients,lower= log(fit$ci.lower),upper= log(fit$ci.upper))
    loglik <- fit$loglik #the null and maximimized (penalized) log likelihood
   }else{ #Do if no firth correction is requested
      fit <- survival::coxph(formula,datac)
      res <- cbind(beta.lp=fit$coefficients,lower= log(summary(fit)$conf.int[,3]),upper= log(summary(fit)$conf.int[,4]))
    loglik <- fit$loglik
    }
  return(list(model=fit, formula=formula,predtable=res,loglik=loglik, firth=firth))
  }