#' Compute the Cindex
#' 
#' @param digits to round the Cindex; if digits=NULL, do not round
#' Produce metrics to evaluate a survival model
#' @export
Cindex <- function(mod, newdat0, digits=3){
  # extract components
  surv.form <- unlist(strsplit(as.character(mod$formula), split='~')[2])
  obj <- decomposeSurvform(mod$formula, newdat0)

  surv <- obj$survpair
  predictors <- obj$predictors
  complete.index <- obj$indcc
  newdat=newdat0[complete.index,]

  # A list of performance indices for survival model
  pred <- survPlus::predictCox(mod$model, newdata=newdat)

  C <- Hmisc::rcorrcens(survival::Surv(newdat[,surv[1]],newdat[,surv[2]]) ~ I(-1 * pred))

  if (is.null(digits)) {
    return(C[1])
  } else {
    return(c(round(C[1],digits)))
  }
}
