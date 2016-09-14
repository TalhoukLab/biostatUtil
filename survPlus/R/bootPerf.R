#' Assess Performance of a survival model
#'
#' Produce metrics to evaluate a survival model in Bootstrap samples (Internal
#' Validation)
#'
#' @param dat0 is the training data
#' @param var.names are the names of the predictors
#' @param is the time variable
#' @param event.ind is the event indicator
#' @param B is the number of bootstrap iterations defaults to 1000
#' @param seed is a random number generator seed set to 2014
#' @param firth whether or not to perform a firth correction set to TRUE
#' @return
#' \item{apparent}{the apparent fit: testing the model on the full training
#' data}
#' \item{boot}{the bootstrap fit: testing the model on the bootstrapped training
#' data}
#' \item{boot632}{the bootstrap 0.632 fit: testing the model on the bootstrapped
#' test data, ie the data that was not selected for bootstrap training}
#' @author Aline Talhouk
#' @export
bootPerf <- function(dat0, var.names, time.var, event.ind,
                     B = 1000, seed = 2014, firth = TRUE) {
  set.seed(seed)

  #Select relevant variables and remove variables with missing observations
  dat <- na.omit(dat0[, c(time.var, event.ind, var.names)])
  #Generate boostrap samples
  boots <- genBootSpls(dat, B)

  #Construct modeling formula
  mod.form <- as.formula(paste("survival::Surv(", time.var, ",", event.ind,
                               ")" , "~", paste(var.names, collapse = " + ")))

  #Apparent fit (testing the model on the full training data (dat))
  app.mod.fit <- survPlus::doCox(mod.form, data = dat, firth = firth)
  perf.app <- survPlus::Cindex(app.mod.fit, dat)

  #Bootstrap fit
  boot.mod <- lapply(boots$boot.tr, function(x)
    survPlus::doCox(mod.form, data = x, firth = firth))

  ## Standard bootstrap (test the model on the bootstrapped training data)
  perf.boot <- mapply(function(X, Y) {
    Cindex(X, newdat0 = Y)
  },
  X = boot.mod, Y = boots$boot.tr, SIMPLIFY = FALSE)

  ## .632 bootstrap (test the model on the bootstrapped test data)
  perf.632 <- mapply(function(X, Y) {
    Cindex(X, newdat = Y)
  },
  X = boot.mod, Y = boots$boot.te, SIMPLIFY = FALSE)

  return(list(apparent = perf.app,
              boot = unlist(perf.boot),
              boot632 = unlist(perf.632)))
}