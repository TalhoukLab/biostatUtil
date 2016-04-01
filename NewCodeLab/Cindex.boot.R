# FUNCTIONS
#rm(list = ls(all = TRUE))  # Clean up
library(survAUC)
library(ECClocal)
library(Hmisc)
library(survcomp)
library(clinfun)
library(coxphf)
library(magrittr)
library(dplyr)
library(reshape2)
library(tidyr)

runBootValidation <- function(data, modelVars,time.var,event.ind,B, modelName, dataName){  
  CbootSample <- mapply(function(X,Y){bootPerf(data,modelVars, X,Y,B)$boot632}, X=time.var, Y=event.ind, SIMPLIFY = T) %>%
    set_colnames(c("OS","DSS","PFS"))%>%
    melt(.)%>% 
    set_colnames(c("BootSample", "Endpoint", "Cindex")) %>% 
    data.frame(Model=rep(modelName,B),Data=rep(dataName,B),.)
  return(CbootSample=CbootSample)
}



error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
  if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
    stop("vectors must be same length")
  arrows(x,upper, x, lower, angle=90, code=3, length=length, ...)
}



dat0<-emdb.all#subset(emdb.hyst,cohort=="BJC")
dat0$os.sts <- as.numeric(dat0$os.sts=="os.event")
dat0$dss.sts <- as.numeric(dat0$dss.sts=="dss.event")
dat0$rfs.sts <- as.numeric(dat0$rfs.sts=="rfs.event")

time.var <- c("os.yrs","dss.yrs","rfs.yrs")
event.ind <- c("os.sts","dss.sts","rfs.sts")

B <- 1000


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

write.csv(df1, "bootCindex.csv")

bootCI <- function(x) quantile(x,c(0.5,0.025,0.975), names=F, na.rm = TRUE)

#agg.dat <-dcast(Cindex~Model+Endpoint+Data,bootCI, data=df1)

by_model <- group_by(df1,Model, Data, Endpoint)

y.mean <- summarise(by_model,CindexMedian=median(Cindex, na.rm = T), lower=quantile(Cindex, c(0.025),na.rm=TRUE), upper=quantile(Cindex, c(0.975),na.rm=TRUE)) %>% filter(Data=="ALL") %>% select(Endpoint,Model, CindexMedian) %>% spread(Endpoint,CindexMedian)

y.lower<- summarise(by_model,CindexMedian=median(Cindex, na.rm = T), lower=quantile(Cindex, c(0.025),na.rm=TRUE), upper=quantile(Cindex, c(0.975),na.rm=TRUE)) %>% filter(Data=="ALL") %>% select(Endpoint,Model, lower) %>% spread(Endpoint,lower)

y.upper<- summarise(by_model,CindexMedian=median(Cindex, na.rm = T), lower=quantile(Cindex, c(0.025),na.rm=TRUE), upper=quantile(Cindex, c(0.975),na.rm=TRUE)) %>% filter(Data=="ALL") %>% select(Endpoint,Model, upper) %>% spread(Endpoint,upper)


barx <- barplot(as.matrix(y.mean[,3:5]), beside=TRUE,col=c("blue","magenta","green"), ylim=c(0,1.5), names.arg=c("OS", "DSS", "PFS"), axis.lty=1, xlab="Endpoint", ylab="C-Index (Median)", legend.text = as.character(y.mean$Model), main="ALL")
abline(h=0.5)

error.bar(barx,as.matrix(y.mean[,3:5]),as.matrix(y.upper[,3:5]),as.matrix(y.lower[,3:5]))

y.mean <- summarise(by_model,CindexMedian=median(Cindex, na.rm = T), lower=quantile(Cindex, c(0.025),na.rm=TRUE), upper=quantile(Cindex, c(0.975),na.rm=TRUE)) %>% filter(Data=="BJC") %>% select(Endpoint,Model, CindexMedian) %>% spread(Endpoint,CindexMedian)

y.lower<- summarise(by_model,CindexMedian=median(Cindex, na.rm = T), lower=quantile(Cindex, c(0.025),na.rm=TRUE), upper=quantile(Cindex, c(0.975),na.rm=TRUE)) %>% filter(Data=="BJC") %>% select(Endpoint,Model, lower) %>% spread(Endpoint,lower)

y.upper<- summarise(by_model,CindexMedian=median(Cindex, na.rm = T), lower=quantile(Cindex, c(0.025),na.rm=TRUE), upper=quantile(Cindex, c(0.975),na.rm=TRUE)) %>% filter(Data=="BJC") %>% select(Endpoint,Model, upper) %>% spread(Endpoint,upper)


barx <- barplot(as.matrix(y.mean[,3:5]), beside=TRUE,col=c("blue","magenta","green"), ylim=c(0,1.5), names.arg=c("OS", "DSS", "PFS"), axis.lty=1, xlab="Endpoint", ylab="C-Index (Median)", legend.text = as.character(y.mean$Model), main="BJC")
abline(h=0.5)

error.bar(barx,as.matrix(y.mean[,3:5]),as.matrix(y.upper[,3:5]),as.matrix(y.lower[,3:5]))


