#' Find cutpoint by coxph
#' @export
findCutpointByCoxph <- function(input.d,surv.formula){
  # make sure is univariable formula 
  if (length(surv.formula[[3]]) > 1) {
    print(paste("ERROR (find.cut.point.by.coxph): unable to process the survival formula: ",deparse(surv.formula)))
    print("NOTE: formula must be of form: Surv(time,status) ~ variable")
    return(NA)
  }
  
  # find variable name
  var.name <- deparse(surv.formula[[3]])
  
  if (!is.numeric(input.d[,var.name])) {
    print(paste("ERROR (find.cut.point.by.coxph): variable (",var.name,") is NOT NUMERIC!!!"))
    return(NA)
  }
  
  # find the min,max value of the variable
  possible.values <- as.numeric(names(table(input.d[,var.name])))
  num.possible.values <- length(possible.values)
  if (num.possible.values < 2) {
    print(paste("ERROR (find.cut.point.by.coxph): variable (",var.name,") do not have any variation (i.e. all same value) for cut point determination."))
    return(NA)
  }
  
  # try all possible cut point
  cut.points <- c()
  aics <- c()
  hrs <- c()
  for (cut.point in possible.values[2:length(possible.values)]) {
    cox.model.fit <- NA
    tryCatch({
      cox.model.fit <- coxph(as.formula(paste(deparse(surv.formula),">=",cut.point)),data=input.d)
    }, warning = function(w) {
      # just ignore silently
    }, error = function(e) {
      print(paste("ERROR (find.cut.point.by.coxph) unexpected error occured:",e))
    }, finally = function(e) {
      # just ingore silently
    })
    if (length(cox.model.fit) > 1) { # cox.model.fit must not be NA, otherwise, length(cox.model.fit) would be 1
      # cox model with no warning/error
      cut.points <- c(cut.points, cut.point)
      aics       <- c(aics,       extractAIC(cox.model.fit)[2])
      hrs        <- c(hrs,        exp(cox.model.fit$coefficients[[1]]))
    }
  }
  
  # return results
  return(list(
    "best.cut.point.by.aic"=cut.points[which.min(aics)],
    "best.cut.point.by.hr"=cut.points[which.max(hrs)],
    "cut.points"=cut.points,
    "aics"=aics,
    "hazard.ratios"=hrs
  ))
}