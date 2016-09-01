#' Find cutpoint by Cox model statistics
#' 
#' Try to find a single cutpoint by maximizing the hazard ratio or minimizing
#' the AIC.
#' 
#' The formula must be univariable.
#' 
#' @param input.d input data, typically a matrix or data frame
#' @param surv.formula a formula of type \code{Surv(time, status) ~ x}, where 
#'   \code{x} is the variable of interest.
#' @return best cutpoint as determined by different metrics. Returns \code{NA} 
#'   if \code{surv.formula} is not univariable, the variable is not numeric, or 
#'   there is no variation in the variable of interest.
#' @section Warning: \code{surv.formula} cannot be multivariable. For example,
#'   \code{Surv(time, status) ~ x + age} won't work but \code{Surv(time, status)
#'   ~ x} is fine.
#' @author Samuel Leung
#' @export
findCutpointByCoxph <- function(input.d, surv.formula){
  if (length(surv.formula[[3]]) > 1) {
    print(paste("ERROR (find.cut.point.by.coxph): unable to process the survival formula: ", deparse(surv.formula)))
    print("NOTE: formula must be of form: Surv(time,status) ~ variable")
    return(NA)
  }

  var.name <- deparse(surv.formula[[3]])
  if (!is.numeric(input.d[, var.name])) {
    print(paste("ERROR (find.cut.point.by.coxph): variable (", var.name, ") is NOT NUMERIC!!!"))
    return(NA)
  }
  
  # find the min,max value of the variable
  possible.values <- as.numeric(names(table(input.d[, var.name])))
  num.possible.values <- length(possible.values)
  if (num.possible.values < 2) {
    print(paste("ERROR (find.cut.point.by.coxph): variable (", var.name, ") do not have any variation (i.e. all same value) for cut point determination."))
    return(NA)
  }
  
  # try all possible cut point
  cut.points <- aics <- hrs <- c()
  for (cut.point in possible.values[2:length(possible.values)]) {
    cox.model.fit <- NA
    tryCatch({
      cox.model.fit <- coxph(as.formula(paste(deparse(surv.formula), ">=", cut.point)), data = input.d)
    }, warning = function(w) {
      # just ignore silently
    }, error = function(e) {
      print(paste("ERROR (find.cut.point.by.coxph) unexpected error occured:", e))
    }, finally = function(e) {
      # just ingore silently
    })
    if (length(cox.model.fit) > 1) { # cox.model.fit must not be NA, otherwise, length(cox.model.fit) would be 1
      # cox model with no warning/error
      cut.points <- c(cut.points, cut.point)
      aics <- c(aics, extractAIC(cox.model.fit)[2])
      hrs <- c(hrs, exp(cox.model.fit$coefficients[[1]]))
    }
  }
  return(list("best.cut.point.by.aic" = cut.points[which.min(aics)],
              "best.cut.point.by.hr" = cut.points[which.max(hrs)],
              "cut.points" = cut.points,
              "aics" = aics,
              "hazard.ratios" = hrs))
}