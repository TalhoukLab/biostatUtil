# FUNCTIONS
rm(list = ls(all = TRUE))  # Clean up
library(survAUC)
library(ECClocal)
library(Hmisc)
library(survcomp)
library(clinfun)
library(coxphf)

genBootSpls <- function(dat, B=1000){
  N <- nrow(dat)
  boot.dat <- plyr::rlply(B, {
    tr.ind <- sample(1:N, replace = TRUE)
    te.ind <- which(!(1:N %in% tr.ind))
    dat.train <- dat[tr.ind, ]
    dat.test <- dat[te.ind, ]
    list(tr = dat.train, te = dat.test)
  })
  
  boot.tr <- lapply(boot.dat, function(x) x$tr) 
  boot.te <- lapply(boot.dat, function(x) x$te)
  return(list(boot.tr=boot.tr,boot.te=boot.te))
}


survPerfInd <- function(mod.pred,surv.time,surv.event){
#  surv.form <- Surv(surv.time,surv.event)
# OXS.i <- OXS(surv.form, mod.pred, null.pred)
#  NR2.i <-Nagelk(surv.form, mod.pred, null.pred)
#  XO.i <-XO(surv.form, mod.pred, null.pred)
#  C <- rcorrcens(surv.form ~ I(-1 * mod.pred), data = dat)
  perf <- concordance.index(x = mod.pred, surv.time, 
                            surv.event, method = "noether")
  return(round(unlist(perf[1:6]),3))
  
#  coxphCPE(app.mod.fit)
}




bootPerf <- function(dat, var.names, time.var, event.ind, B=1000, seed=2014 ){
  
set.seed(seed)

dat <- na.omit(dat0[,c(time.var,event.ind,var.names)]) 
boots <- genBootSpls(dat,B)


mod.form <- as.formula(paste("Surv(",time.var,",",event.ind,")" ,"~", paste(var.names, collapse = " + ")))
mod.null<- as.formula(paste("Surv(",time.var,",",event.ind,")" ,"~", 1))


bdat <- boots$boot.tr[[1]]
bt <-  boots$boot.te[[1]]

app.null.fit<- coxph(mod.null,data = dat)
null.pred <- predict(app.null.fit,newdata=dat)

app.mod.fit <- coxph(mod.form,data = dat)
app.mod.pred <- predict(app.mod.fit, newdata=dat)
app.perf <- survPerfInd(app.mod.pred, dat[,time.var], dat[,event.ind])

boot.mod<- lapply(boots$boot.tr, function(x) coxph(mod.form,data = x))
app.boot.mod.fit <- mapply(function(X,Y){predict(X, newdata=Y)}, 
                           X = boot.mod,Y = boots$boot.tr, SIMPLIFY = F)
app.boot.perf <- t(mapply(function(x,y) survPerfInd(x,y[,time.var], y[,event.ind]),
                   x=app.boot.mod.fit, y=boots$boot.tr))

test.632.mod.fit <- mapply(function(X,Y){predict(X, newdata=Y)}, 
                           X = boot.mod,Y = boots$boot.te)
test.632.perf <- t(mapply(function(x,y) survPerfInd(x,y[,time.var], y[,event.ind]),
                          x=test.632.mod.fit, y=boots$boot.te))

test.mod.fit <- lapply(boot.mod, function(x)predict(x, newdata=dat))
test.perf <- t(sapply(test.mod.fit, 
                      function(x) survPerfInd(x,dat[,time.var], dat[,event.ind])))
pred.opt <- app.boot.perf[,1]-test.632.perf[,1]

return(list(apparent=app.perf, boot632=quantile(test.632.perf[,1], c(0.025,0.5,0.975),na.rm = T), bootstrapAdj=quantile(rep(app.perf[1],B)-pred.opt, c(0.025,0.5,0.975),na.rm = T)))
}


#INPUT PARAMETERS
emdb.hyst <- subset(emdb.all, tissue.source=='surgery')


dat0<-emdb.hyst#subset(emdb.hyst,cohort=="BJC")
dat0$os.sts <- as.numeric(dat0$os.sts=="os.event")
dat0$dss.sts <- as.numeric(dat0$dss.sts=="os.event")
dat0$rfs.sts <- as.numeric(dat0$rfs.sts=="os.event")

#var.names <- c("age.at.surgery",  "stage_gr", "histology.gr", "LVSI", "nodes", 
#               "any.treatment","Myo.invasion","EClass")

var.names <- c("EClass")
time.var <- "os.yrs"
event.ind <- "os.sts"

B <- 1000

esmo <- bootPerf(dat, var.names=c("esmo","any.treatment"), time.var, event.ind)
esmoMol <- bootPerf(dat, var.names=c("esmo","EClass","any.treatment"), time.var, event.ind)
clinpath <- bootPerf(dat, var.names=c("age.at.surgery","bmi","histology.gr","any.treatment"), time.var, event.ind)

clinpathMol <- bootPerf(dat, var.names=c("age.at.surgery","bmi","histology.gr","any.treatment","EClass"), time.var, event.ind)
