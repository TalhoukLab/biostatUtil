#' Coxph with nicer output
#' @export
prettyCoxph <- function(input.formula, input.d, use.firth=1, check.ph=FALSE, ph.test.plot.filename="no.file", ...) {
  
  # set local variable in environment searchable by local function calls
  assign(".my.formula",input.formula, pos=1) 
  assign(".my.data",input.d, pos=1)
  
  # figure out if percentage of censored cases large enough to use Firth
  ok.to.use.firth <- ifelse(use.firth == -1, TRUE, FALSE)
  if (use.firth < 1 & use.firth > -1) { # no need to check if use.firth is 1 or -1
    all.vars(.my.formula)[2]
    for (var.name in  all.vars(.my.formula)[-c(1,2)]) {
      if (is.factor(.my.data[,var.name])) {
        # only need to check if its a factor
        fit <- survfit(as.formula(paste(deparse(.my.formula[[2]]),"~",var.name)),data=.my.data)
        for (i in 1:nrow(fit)) {
          if((sum(fit[i]$n.censor)/fit[i]$n) > use.firth) {
            ok.to.use.firth <- TRUE
            break # no need to check further
          }
        }
        if (ok.to.use.firth) {break} # no need to check further
      }
    }
  } 
  
  if (ok.to.use.firth) {
    fit.firth <- coxphf(.my.formula, data=.my.data, ...)	
    fit.firth$nevent <- sum(fit.firth$y[,"status"]) # coxphf fit object does not have nevent
  } else {
    fit.firth <- NA
  }
  fit <- coxph(.my.formula, data=.my.data, ...) # fit is coxph ALWAYS!!!
  .my.formula <- fit$formula
  ph.check <- "NOT CALCULATED"
  
  if (check.ph) {
    
    ph.test <- cox.zph(fit)
    
    ph.check <- matrix(ph.test$table[rownames(ph.test$table)!="GLOBAL", 3],
                       nrow=length(names(fit$coefficients)), 
                       ncol=1,
                       dimnames=c(list(names(fit$coefficients)),list(c('PH test')))
    )		
    
    if (!is.na(ph.test.plot.filename)) {
      if (ph.test.plot.filename!="no.file") {
        pdf(ph.test.plot.filename)
      }
      plot(ph.test)
      if (ph.test.plot.filename!="no.file") {
        dev.off()
      }
    }
  }
  
  if (ok.to.use.firth) {
    # coxphf fit object
    result <- cbind(
      matrix(cbind(exp(fit.firth$coefficients),fit.firth$ci.lower,fit.firth$ci.upper), 
             nrow=length(names(fit.firth$coefficients)), 
             ncol=3, 
             dimnames=c(list(names(fit.firth$coefficients)),list(c('exp(coef)','lower .95', 'upper .95')))
      ),
      matrix( fit.firth$prob,
              nrow=length(names(fit.firth$coefficients)), 
              ncol=1,
              dimnames=c(list(names(fit.firth$coefficients)),list(c('Pr(>|z|)')))
      ),
      ph.check
    )	
  } else {
    # coxph fit object
    result <- cbind(
      matrix(summary(fit)$conf.int[,c('exp(coef)','lower .95', 'upper .95')], 
             nrow=length(names(fit$coefficients)), 
             ncol=3, 
             dimnames=c(list(names(fit$coefficients)),list(c('exp(coef)','lower .95', 'upper .95')))
      ),
      matrix(summary(fit)$coefficients[,'Pr(>|z|)'],
             nrow=length(names(fit$coefficients)), 
             ncol=1,
             dimnames=c(list(names(fit$coefficients)),list(c('Pr(>|z|)')))
      ),
      ph.check
    )
  }
  result.colnames <- colnames(result)
  #result.colnames[c(ncol(result)-1,ncol(result))] <- c('p-value','cox.zph p-value')
  colnames(result) <- result.colnames
  
  if (check.ph) {
    return.obj        <- list(result,   fit,   fit.firth, fit$n, fit$nevent, ph.test,  ok.to.use.firth)
    names(return.obj) <- c(   "output", "fit", "fit.firth", "n",   "nevent",   "ph.test","used.firth")
  } else {
    return.obj        <- list(result,   fit,   fit.firth,fit$n, fit$nevent,ok.to.use.firth)
    names(return.obj) <- c(   "output", "fit", "fit.firth", "n",   "nevent"  ,"used.firth")
  }
  
  return (return.obj)
}