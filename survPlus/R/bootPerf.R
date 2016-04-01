#' Assess Performance
#'
#' Produce metrics to evaluate a survival model in Bootstrap samples
#' @export

bootPerf <- function(dat0, var.names, time.var, event.ind, B=1000, seed=2014, firth=TRUE){
  set.seed(seed)

  dat <- na.omit(dat0[,c(time.var,event.ind,var.names)])
  boots <- genBootSpls(dat,B)

  mod.form <- as.formula(paste("Surv(",time.var,",",event.ind,")" ,"~", paste(var.names, collapse = " + ")))

  #Apparent fit
  app.mod.fit <- survival::doCox(mod.form, data = dat, firth = firth)
  perf.app <- Cindex(app.mod.fit, dat)

  #Bootstrap fit
  boot.mod<- lapply(boots$boot.tr, function(x) survival::doCox(mod.form,data = x, firth = firth))

  ## Standard bootstrap
  perf.boot <- mapply(function(X,Y){Cindex(X, newdat0=Y)},
                             X = boot.mod,Y = boots$boot.tr, SIMPLIFY = F)
  ## .632 bootstrap
  perf.632 <-mapply(function(X,Y){Cindex(X, newdat=Y)},
                    X = boot.mod,Y = boots$boot.te, SIMPLIFY = F)

  #pred.opt <- perf.boot[,1]-perf.test[,1]
  #bootAdj=rep(perf.app[1],B)-pred.opt
  return(list(apparent=perf.app, boot=unlist(perf.boot), boot632=unlist(perf.632)))
}

