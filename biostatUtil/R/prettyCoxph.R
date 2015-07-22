#' Nicer outputs from coxph model fits
#' 
#' Formats the coxph (or coxphf) model outputs into a prettier display.
#' 
#' The original fit call is returned, along with other useful statistics.
#' 
#' @param input.formula a formula object, with the response on the left of a
#' \code{~} operator, and the terms on the right. The response must be a
#' survival object as returned by the \code{Surv} function.
#' @param input.d data.frame containing the variables in \code{input.formula}
#' @param use.firth if set to -1, Firth's correction is applied using
#' \code{coxphf}. The default value is 1, which uses \code{coxph}.
#' @param check.ph if \code{TRUE}, checks the proportional hazard assumption.
#' @param ph.test.plot.filename filename of residual plot. If set to
#' \code{no.file} (default), plots to console. If \code{NA}, no plot will be
#' generated.
#' @param ... additional arguments to \code{coxph} or \code{coxphf}
#' @return A list with elements
#' \item{output}{A character matrix showing the hazard ratio, confidence bounds,
#' and p-value for each coefficient. If \code{check.ph = TRUE}, the last column
#' shows the results of the PH test, otherwise it reads "NOT CALCULATED" for
#' every coefficient.}
#' \item{fit}{The fit output call returned from \code{coxph} or \code{coxphf}}
#' \item{n}{The number of observations used in the fit}
#' \item{nevent}{The number of events used in the fit}
#' \item{ph.test}{The rho, Chi-squared statistic, and p-value for each coefficient.
#' Not shown if \code{check.ph = FALSE}}
#' \item{used.firth}{logical; if \code{TRUE}, Firth's correction was applied
#' using \code{coxphf}}
#' @author Samuel Leung, Derek Chiu
#' @import survival
#' @export
prettyCoxph <- function(input.formula, input.d, use.firth = 1,
                        check.ph = FALSE,
                        ph.test.plot.filename = "no.file", ...) {
  
  # set local variable in environment searchable by local function calls
  pos <- 1
  .my.data <- input.d
  assign(".my.formula", input.formula, envir = as.environment(pos)) 
  assign(".my.data", input.d, envir = as.environment(pos))
  
  # figure out if percentage of censored cases large enough to use Firth
  ok.to.use.firth <- ifelse(use.firth == -1, TRUE, FALSE)
  if (use.firth < 1 & use.firth > -1) { # no need to check if use.firth is 1 or -1
    all.vars(.my.formula)[2]
    for (var.name in all.vars(.my.formula)[-c(1, 2)]) {
      if (is.factor(.my.data[, var.name])) {
        # only need to check if its a factor
        fit <- survival::survfit(as.formula(paste(deparse(.my.formula[[2]]),
                                                  "~", var.name)),
                                 data = .my.data)
        for (i in 1:nrow(fit)) {
          if((sum(fit[i]$n.censor) / fit[i]$n) > use.firth) {
            ok.to.use.firth <- TRUE
            break # no need to check further
          }
        }
        if (ok.to.use.firth) {
          break
        } # no need to check further
      }
    }
  } 
  
  if (ok.to.use.firth) {
    fit.firth <- coxphf::coxphf(.my.formula, data = .my.data, ...)	
    fit.firth$nevent <- sum(fit.firth$y[, "status"]) # coxphf fit object does not have nevent
  } else {
    fit.firth <- NA
  }
  fit <- survival::coxph(.my.formula, data = .my.data, ...) # fit is coxph ALWAYS!!!
  .my.formula <- fit$formula
  ph.check <- "NOT CALCULATED"
  
  if (check.ph) {
    
    ph.test <- cox.zph(fit)
    
    ph.check <- matrix(ph.test$table[rownames(ph.test$table) != "GLOBAL", 3],
                       nrow = length(names(fit$coefficients)), 
                       ncol = 1,
                       dimnames = c(list(names(fit$coefficients)), list(c('PH test')))
    )		
    
    if (!is.na(ph.test.plot.filename)) {
      if (ph.test.plot.filename != "no.file") {
        pdf(ph.test.plot.filename)
      }
      plot(ph.test)
      if (ph.test.plot.filename != "no.file") {
        dev.off()
      }
    }
  }
  
  if (ok.to.use.firth) {
    # coxphf fit object
    result <- cbind(
      matrix(cbind(exp(fit.firth$coefficients), fit.firth$ci.lower,fit.firth$ci.upper), 
             nrow = length(names(fit.firth$coefficients)), 
             ncol = 3, 
             dimnames = c(list(names(fit.firth$coefficients)), list(c('exp(coef)', 'lower .95', 'upper .95')))
      ),
      matrix(fit.firth$prob,
             nrow = length(names(fit.firth$coefficients)), 
             ncol = 1,
             dimnames = c(list(names(fit.firth$coefficients)), list(c('Pr(>|z|)')))
      ),
      ph.check
    )	
  } else {
    # coxph fit object
    result <- cbind(
      matrix(summary(fit)$conf.int[, c('exp(coef)', 'lower .95', 'upper .95')],
             nrow = length(names(fit$coefficients)), 
             ncol = 3, 
             dimnames = c(list(names(fit$coefficients)),
                          list(c('exp(coef)', 'lower .95', 'upper .95')))),
      matrix(summary(fit)$coefficients[, 'Pr(>|z|)'],
             nrow = length(names(fit$coefficients)), 
             ncol = 1,
             dimnames = c(list(names(fit$coefficients)), list(c('Pr(>|z|)')))),
      ph.check
    )
  }
  result.colnames <- colnames(result)
  #result.colnames[c(ncol(result)-1,ncol(result))] <- c('p-value','cox.zph p-value')
  colnames(result) <- result.colnames
  
  if (check.ph) {
    return.obj        <- list(result,   fit,   fit.firth, fit$n, fit$nevent, ph.test,  ok.to.use.firth)
    names(return.obj) <- c(   "output", "fit", "fit.firth", "n",   "nevent",   "ph.test", "used.firth")
  } else {
    return.obj        <- list(result,   fit,   fit.firth, fit$n, fit$nevent, ok.to.use.firth)
    names(return.obj) <- c(   "output", "fit", "fit.firth", "n",   "nevent"  ,"used.firth")
  }
  return(return.obj)
}