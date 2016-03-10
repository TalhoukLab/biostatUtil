# FUNCTIONS
rm(list = ls(all = TRUE))  # Clean up
library(survAUC)
library(ECClocal)
library(Hmisc)
library(survcomp)
library(clinfun)
library(coxphf)
library(magrittr)
library(dplyr)
library(reshape2)
##################
# Helper functions
##################

genBootSpls <- function(dat, B=1000){
  # Generate Bootstrap Samples by sampling with replacement 
  # the bootstrap samples are saved in a list called tr
  # the unselected samples are save in a list called te
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
# A list of performance indices for survival model
 # C <- rcorrcens(surv.form ~ I(-1 * mod.pred), data = dat)
  perf <- concordance.index(x = mod.pred, surv.time, 
                            surv.event, method = "conservative")
  return(round(unlist(perf[1:6]),3))
  
#  coxphCPE(app.mod.fit)
}

error.bar <- function(x,g, ylim = range(c(0,l,u)),...)
{
  m <- by(x,g,mean)
  SD <- by(x,g,sd)
  n <- by(x,g,length)
  l <- quantile(m,0.025)
  u <- quantile(m, 0.975)
  b <- barplot(m,ylim=ylim,...)
  arrows(x0=b,x1=b,y0=l,y1=u,code=3,angle=90)
  abline(h=0.5,lty=2)
}



bootPerf <- function(dat0, var.names, time.var, event.ind, B=1000, seed=2014 ){
  
set.seed(seed)

dat <- na.omit(dat0[,c(time.var,event.ind,var.names)]) 
boots <- genBootSpls(dat,B)

mod.form <- as.formula(paste("Surv(",time.var,",",event.ind,")" ,"~", paste(var.names, collapse = " + ")))
mod.null<- as.formula(paste("Surv(",time.var,",",event.ind,")" ,"~", 1))


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

return(list(apparent=app.perf, boot632=test.632.perf[,1], bootstrapAdj=rep(app.perf[1],B)-pred.opt))
}


#INPUT PARAMETERS
emdb.hyst <- subset(emdb.all, tissue.source=='surgery')


dat0<-emdb.hyst#subset(emdb.hyst,cohort=="BJC")
dat0$os.sts <- as.numeric(dat0$os.sts=="os.event")
dat0$dss.sts <- as.numeric(dat0$dss.sts=="dss.event")
dat0$rfs.sts <- as.numeric(dat0$rfs.sts=="rfs.event")

time.var <- c("os.yrs","dss.yrs","rfs.yrs")
event.ind <- c("os.sts","dss.sts","rfs.sts")

B <- 1000

esmo_all <- 
  
  data=dat0;
modelVars=c("esmo", "any.treatment")
modelName="esmo"
dataName="all"
B=1000
  
runBootValidation <- function(data, modelVars,time.var,event.ind,B, modelName, dataName){  
  CbootSample <- mapply(function(X,Y){bootPerf(data,modelVars, X,Y,B)$boot632}, X=time.var, Y=event.ind, SIMPLIFY = T) %>%
  set_colnames(c("OS","DSS","PFS"))%>%
    melt(.)%>% 
    set_colnames(c("BootSample", "Endpoint", "Cindex")) %>% 
    data.frame(Model=rep(modelName,B),Data=rep(dataName,B),.)
  return(CbootSample=CbootSample)
  }

esmo_all <- runBootValidation(dat0, c("esmo", "any.treatment") , time.var , event.ind ,B,"ESMO","ALL") 
esmo_bjc <-runBootValidation(subset(dat0,cohort=="BJC"), c("esmo", "any.treatment"), 
                             time.var , event.ind ,B,"ESMO","BJC")

esmo_val <-runBootValidation(subset(dat0,cohort=="VAL"), c("esmo", "any.treatment"), 
                             time.var , event.ind ,B,"ESMO","VAL")

ProMisE_all <- runBootValidation(dat0, c("EClass", "any.treatment") , time.var , event.ind ,B,"PROMISE","ALL") 


ProMisE_bjc <-runBootValidation(subset(dat0,cohort=="BJC"), c("EClass", "any.treatment"), 
                             time.var , event.ind ,B, "PROMISE","BJC")

ProMisE_val <-runBootValidation(subset(dat0,cohort=="VAL"), c("EClass", "any.treatment"), 
                             time.var , event.ind ,B,"PROMISE","VAL")

esmoProMisE_all <- runBootValidation(dat0, c("esmo","EClass", "any.treatment"), 
                                     time.var , event.ind ,B,"ESMO+PROMISE","ALL")


esmoProMisE_bjc <- runBootValidation(subset(dat0,cohort=="BJC"), 
                                     c("esmo","EClass", "any.treatment"), 
                                     time.var , event.ind ,B,"ESMO+PROMISE","BJC")

esmoProMisE_val <- runBootValidation(subset(dat0,cohort=="VAL"), 
                                     c("esmo","EClass", "any.treatment"), 
                                     time.var , event.ind ,B,"ESMO+PROMISE","VAL")

df1 <- rbind(esmo_all,esmo_bjc,esmo_val,
             ProMisE_all,ProMisE_bjc,ProMisE_val,
             esmoProMisE_all,esmoProMisE_bjc,esmoProMisE_val)


bootCI <- function(x) quantile(x,c(0.5,0.025,0.975), na.rm = TRUE)

aggregate(Cindex~Model+Data,bootCI, data=df1)

