#' Compute the Cindex
#'
#' Produce metrics to evaluate a survival model
#' @export
Cindex <- function(mod, newdat0){
  # extract components
  surv.form <- unlist(strsplit(as.character(mod$formula), split='~')[2])
  obj <- decomposeSurvform(mod$formula, newdat0)

  surv <- obj$survpair
  predictors <- obj$predictors
  complete.index <- obj$indcc
  newdat=newdat0[complete.index,]

  # A list of performance indices for survival model
  pred <- predictCox(mod, newdata=newdat)

  C <- Hmisc::rcorrcens(Surv(newdat[,surv[1]],newdat[,surv[2]]) ~ I(-1 * pred))

 return(c(round(C[1],3)))

}
