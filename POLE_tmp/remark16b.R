#dmat input data matrix 
#dmat=dat1
#event="OS"
#event is the type of survival event we are studying labeled OS.yrs and OS.sts 
coxphOut <- function (x,coefnames=NULL,digits=2,use.firth=FALSE) {
  cox <- x # this can be either an object from coxph() or coxphf() ... slightly different output
  Coef <- cox$coef
  se <- sqrt(diag(cox$var))
  CI   <- exp(confint(cox))
  n <- cox$n #summary(cox)$n
  events <- ifelse(use.firth,sum(cox$y[,"status"]),cox$nevent) # events <- summary(cox)$nevent
  Z<- Coef/se
  p    <- 1 - pchisq((Coef/se)^2, 1)
  tmp <- cbind(n,events,Coef,se, Z,p, HR= exp(Coef),CI)
  if(is.null(coefnames)){coefnames=names(Coef)}
  dimnames(tmp) <- list(coefnames, c("n","events" ,"coef "," se "," Z-Score "," P-value "," HR ", " 2.5% ", " 97.5% "))
  return(round(tmp,digits))  
}
printCoxMod<-function(cox,Capt){
  TAB=htmlTable(cox, 
                rowlabel="Predictors", 
                #rgroupCSSstyle="", rgroupCSSseparator="", 
                caption=Capt, 
                ctable=TRUE)
  pander(TAB, style='rmarkdown')
}
printInterModels<- function(mod1,mod2,mod3,Capt){
  vars <- rbind(mod1,mod2,mod3)
  
  cgroup <- c("Coefficients","HR and [95% CI]")
  n.cgroup <- c(4,3)
  rgroup <- c("Model 1", "Model 2", "Model 3")
  n.rgroup <- c(nrow(mod1), nrow(mod2), nrow(mod3))
  TAB=htmlTable(vars, 
                rowlabel="Models Considered", 
                rgroup=rgroup, n.rgroup = n.rgroup, 
                #rgroupCSSstyle="", rgroupCSSseparator="", 
                caption=paste(Capt,"Models <sup>&dagger;</sup> testing for interactions",sep=""), 
                tfoot="<sup>&dagger;</sup> Model 1 includes POLE only, Model 2 includes POLE adjusted for treatment, and Model 3 includes POLE, treatment and an interaction",
                ctable=TRUE)
  pander(TAB, style='rmarkdown')
  
  
  
}
# per email from Aline 2015-05-14 ... use Firth correction in interaction test
# WARNING!!! coxphftest DOES NOT WORK WITH factor !!! need to change all factor to numeric!!!
testInter<-function(dmat,event, labs=c("POLE wt","Rx","POLE wt * Rx"), use.firth=FALSE){
	dmat$POLE <- as.numeric(dmat$POLE)-1
	dmat$Rx <- as.numeric(dmat$Rx)-1
	if (use.firth) {
		switch(event,
			OS={
				cox1=coxphf(Surv(OS.yrs,OS.sts=="os.event")~POLE,data=dmat)
				cox2=coxphf(Surv(OS.yrs,OS.sts=="os.event")~POLE+Rx,data=dmat)
				cox3=coxphf(Surv(OS.yrs,OS.sts=="os.event")~POLE*Rx,data=dmat)
			},
			DSS={
				cox1=coxphf(Surv(DSS.yrs,DSS.sts=="dss.event")~POLE,data=dmat)         
				cox2=coxphf(Surv(DSS.yrs,DSS.sts=="dss.event")~POLE+Rx,data=dmat)
				cox3=coxphf(Surv(DSS.yrs,DSS.sts=="dss.event")~POLE*Rx,data=dmat)  		
			},
			RFS={
				cox1=coxphf(Surv(RFS.yrs,RFS.sts=="rfs.event")~POLE,data=dmat)
				cox2=coxphf(Surv(RFS.yrs,RFS.sts=="rfs.event")~POLE+Rx,data=dmat)
				cox3=coxphf(Surv(RFS.yrs,RFS.sts=="rfs.event")~POLE*Rx,data=dmat)         
			}
		)
	} else {
		switch(event,
    		OS={
        		cox1=coxph(Surv(OS.yrs,OS.sts=="os.event")~POLE,data=dmat)
        		cox2=coxph(Surv(OS.yrs,OS.sts=="os.event")~POLE+Rx,data=dmat)
        		cox3=coxph(Surv(OS.yrs,OS.sts=="os.event")~POLE*Rx,data=dmat)
        	},
       		DSS={
         		cox1=coxph(Surv(DSS.yrs,DSS.sts=="dss.event")~POLE,data=dmat)         
         		cox2=coxph(Surv(DSS.yrs,DSS.sts=="dss.event")~POLE+Rx,data=dmat)
         		cox3=coxph(Surv(DSS.yrs,DSS.sts=="dss.event")~POLE*Rx,data=dmat)  
       		},
       		RFS={
        		cox1=coxph(Surv(RFS.yrs,RFS.sts=="rfs.event")~POLE,data=dmat)
        		cox2=coxph(Surv(RFS.yrs,RFS.sts=="rfs.event")~POLE+Rx,data=dmat)
        		cox3=coxph(Surv(RFS.yrs,RFS.sts=="rfs.event")~POLE*Rx,data=dmat)         
       		}
		)
	}
	mod1=coxphOut(cox1,coefnames=labs[1],  use.firth=use.firth)
	mod2=coxphOut(cox2,coefnames=labs[1:2],use.firth=use.firth)
	mod3=coxphOut(cox3,coefnames=labs,     use.firth=use.firth)
	if (use.firth) {
		lrt1.p=coxphftest(cox2$formula, test=~POLE, data=dmat)$prob
		lrt2.p=coxphftest(cox3$formula, test=~POLE+Rx, data=dmat)$prob
	} else {
		lrt1.p=anova(cox2,cox1)["P(>|Chi|)"][2,]
		lrt2.p=anova(cox3,cox2)["P(>|Chi|)"][2,]
	}

	return(list("mod1"=mod1,"mod2"=mod2,"mod3"=mod3,"lrt1.p"=lrt1.p,"lrt2.p"=lrt2.p))
}


