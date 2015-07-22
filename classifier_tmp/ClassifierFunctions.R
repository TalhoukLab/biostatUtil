###########################
#Functions used
###########################

# some constants used by the functions ...
CLASS.NAME.POLE   <- "POLE"
CLASS.NAME.MSI    <- "MSI"
CLASS.NAME.CNHIGH <- "CNHigh"
CLASS.NAME.CNLOW  <- "CNLow"
CLASS.ORDER <- c(CLASS.NAME.POLE, CLASS.NAME.MSI, CLASS.NAME.CNLOW, CLASS.NAME.CNHIGH)
#CLASS.ORDER <- c(CLASS.NAME.MSI, CLASS.NAME.POLE, CLASS.NAME.CNLOW, CLASS.NAME.CNHIGH)
CLASS.ORDER.CNLOW.AS.REF <- c(CLASS.NAME.CNLOW, CLASS.NAME.POLE, CLASS.NAME.MSI, CLASS.NAME.CNHIGH) 
#CLASS.ORDER.CNLOW.AS.REF <- c(CLASS.NAME.CNLOW, CLASS.NAME.MSI, CLASS.NAME.POLE, CLASS.NAME.CNHIGH) 
CLASS.NAME.REFERENCE <- CLASS.NAME.CNLOW # define the reference group for Cox regression models

CLASS.COLOR.POLE <- "skyblue3"
CLASS.COLOR.MSI <- "orange"
CLASS.COLOR.CNHIGH <- "red"
CLASS.COLOR.CNLOW <- "lightgreen"  
CLASS.COLORS <- c(CLASS.COLOR.POLE, CLASS.COLOR.MSI, CLASS.COLOR.CNLOW, CLASS.COLOR.CNHIGH)

# A function that takes the variable name,
# applies it to the endometrial dataset
# and then runs the results by the TMA variable
getT1Stat <- function(varname,marker){
  getDescriptionStatsBy(varname, marker, 
                        add_total_col=T,
                        show_all_values=T, 
                        hrzl_prop=F,
                        statistics=F, 
                        continuous_fn = describeMean,
						useNA="ifany",#show_missing=TRUE,
                        html=TRUE)
}

getTableData<-function(clinicaldata,byvar,vars,contStatus,labels,useCapt){
  table_data <- list() 
  for (i in 1:length(vars)){
    table_data[[labels[i]]] <- getT1Stat(clinicaldata[,vars[i]],byvar)}
  
  
  rgroup <- c()
  n.rgroup <- c()
  output_data <- NULL
  for (varlabel in names(table_data)){
    output_data <- rbind(output_data, table_data[[varlabel]])
    rgroup <- c(rgroup, varlabel)
    n.rgroup <- c(n.rgroup, nrow(table_data[[varlabel]]))
  }
  
  TAB=htmlTable(output_data, 
                rgroup=rgroup, n.rgroup=n.rgroup, 
				css.rgroup.sep = "",#rgroupCSSseparator="", 
                rowlabel="", 
                caption=useCapt, 
                ctable=TRUE)
  pander(TAB, style = 'rmarkdown')
}
MakeDescriptionTables<-function(dfx,Capt){
  # Summarize a dataset by two variables
  mytable<-xtabs(~Followup+Status+TMA,data=dfx)
  flatTable<-ftable(mytable) # print table 
  t <- as.matrix(flatTable)
  tT<-as.matrix(data.frame(t,Total=rowSums(t)))
  gt<-colSums(tT)
  tTT<-rbind(tT,Total=gt)
  TT<-t(apply(tTT,1, function(x){paste(x,"(",round(x/gt*100,1),"%)",sep="")}));colnames(TT)=c(levels(ed$TMA),"Total")
  t2<-data.frame(TT);
  
  vars<-TT
  rgroup <-c(levels(dfx$Followup),"")
  nrgoup<-c(rep(length(unique(dfx$Status)),length(unique(dfx$Followup))),1)
  htmlTable(vars,rgroup=rgroup,n.rgroup=nrgoup, rowlabel="Status",rgroupCSSstyle="", rgroupCSSseparator="",ctable=TRUE,align=paste(c("c", rep('c',ncol(vars)-1)),collapse=''),  caption  = Capt)
  
}
###
# classify cases: MSI first, then POLE
# - assume input are TRUE/FALSE/NA
EndSub<-function(PolePhen,MSIPhen,CNHPhen){
	#MSI/POLE/CNH/CNL
	Class <- apply(cbind(
			PolePhen, #1
			MSIPhen,  #2
			CNHPhen   #3
		),1,function(x){
		if (is.na(x[2])) {
			return(NA) # unable to go on since MMR is unknown
		} else if (x[2]) {
			return(CLASS.NAME.MSI)
		} else if (is.na(x[1])) { # MMR must be MSS
			return(NA) # unable to go on since POLE is unknown
		} else if (x[1]) {
			return(CLASS.NAME.POLE)
		} else if (is.na(x[3])) { # POLE must be wild type
			return(NA) # unable to go on since FISH/p53 is missing
		} else {
			return(ifelse(x[3],CLASS.NAME.CNHIGH,CLASS.NAME.CNLOW))
		}
	})	
	#Class<-ifelse(MSIPhen,CLASS.NAME.MSI,ifelse(PolePhen,CLASS.NAME.POLE,ifelse(CNHPhen,CLASS.NAME.CNHIGH,CLASS.NAME.CNLOW)))
	return(factor(Class,levels=CLASS.ORDER))
}
###
# classify cases: POLE first, then MSI
# - assume input are TRUE/FALSE/NA
EndSub2<-function(PolePhen,MSIPhen,CNHPhen){
	#POLE/MSI/CNH/CNL
	# this is the order used by TGCA
	Class <- apply(cbind(
			PolePhen, #1
			MSIPhen,  #2
			CNHPhen   #3
		),1,function(x){
		if (is.na(x[1])) {
			return(NA) # unable to go on since POLE is unknown
		} else if (x[1]) {
			return(CLASS.NAME.POLE)
		} else if (is.na(x[2])) { # POLE must be wild type
			return(NA) # unable to go on since MMR is unknown
		} else if (x[2]) {
			return(CLASS.NAME.MSI)
		} else if (is.na(x[3])) { # MMR must be MSS
			return(NA) # unable to go on since FISH/p53 is missing
		} else {
			return(ifelse(x[3],CLASS.NAME.CNHIGH,CLASS.NAME.CNLOW))
		}
	})	
	#Class<-ifelse(PolePhen==1,CLASS.NAME.POLE,ifelse(MSIPhen==1,CLASS.NAME.MSI,ifelse(CNHPhen,CLASS.NAME.CNHIGH,CLASS.NAME.CNLOW)))
	return(factor(Class,levels=CLASS.ORDER))
}

# do KM plot
#
# show.n - whether to show n in plot title
#
PlotKM<-function(Survyrs,Survsts,class,ttl,cols="gcols",legend.pos="topright", show.n=TRUE){
	if (length(cols)==1){
		gcols=c(CLASS.COLOR.POLE, CLASS.COLOR.MSI, CLASS.COLOR.CNLOW, CLASS.COLOR.CNHIGH)
	}else{
		gcols=cols
	}
	d=data.frame(as.numeric(gsub("N/A","",Survyrs)),Survsts,class);colnames(d)=c("years","status","class")
	d=d[complete.cases(d),]
	ix=grep("event",levels(d$status))
	S=Surv(d$years,d$status==levels(d$status)[ix])
  
	p=survdiff(S~d$class)
	fit<-survfit(S~d$class)
	ttl.text <- ttl
	if (show.n) {
		ttl.text <- paste(ttl.text," (n=",sum(fit$n),")",sep="")
	}
	plot(fit, main=ttl.text,lwd=3,col=gcols,xlim=c(0,9))
	legend(legend.pos,levels(factor(class)),text.col=gcols, cex=1,box.lwd=0)
	text(3,0.1,paste("LR p-value: ",round(1 - pchisq(p$chisq,length(p$n) - 1),4), sep=""),cex=1)
}


do.km.plot <- function(
		input.d,
		var.name,
		var.description,
		surv.type,
		line.name=CLASS.ORDER,
		line.color=CLASS.COLORS,
		single.test.type="logrank",
		surv.formula.prefix.os ="Surv(os.yrs,os.sts=='os.event') ~",
		surv.formula.prefix.dss="Surv(dss.yrs,dss.sts=='dss.event') ~",
		surv.formula.prefix.rfs="Surv(rfs.yrs,rfs.sts=='rfs.event') ~",
		...){
	input.d <- input.d[!is.na(input.d[,var.name]),]
	
	surv.formula.prefix <- switch(surv.type,os=surv.formula.prefix.os,dss=surv.formula.prefix.dss,rfs=surv.formula.prefix.rfs)
	# get surv time, status, event name
	surv.time.var.name <- trim.white.spaces(         strsplit(strsplit(surv.formula.prefix,",")[[1]][1],"\\(")[[1]][2])
	surv.sts.var.name  <- trim.white.spaces(         strsplit(strsplit(surv.formula.prefix,",")[[1]][2],"==")[[1]][1])
	surv.event.value   <- trim.white.spaces(strsplit(strsplit(strsplit(surv.formula.prefix,",")[[1]][2],"==")[[1]][2],"'|\"")[[1]][2])
	temp.d <- input.d[!is.na(input.d[,surv.time.var.name]) & !is.na(input.d[,surv.sts.var.name]),]
	temp.d[,surv.time.var.name] <- as.numeric(temp.d[,surv.time.var.name])
	switch(surv.type,
		os={
			plot_km(temp.d,
				as.formula(paste(surv.formula.prefix,var.name)),
				paste(var.description,"(OS; n=",nrow(temp.d),")",sep=""),
				OS.XLAB, # x-axis label
				OS.YLAB, # y-axis label
				line.name, # line names
				line.color=line.color,
				# none = no test shown
				single.test.type=single.test.type, # the test to show if specified show.test="single". 
				# the possible choices are logrank, wilcoxon, taroneware, all
				obs.survyrs=3,
				...
			)
		},
		dss={
			dss.stats <- plot_km(temp.d,
				as.formula(paste(surv.formula.prefix,var.name)),
				paste(var.description,"(DSS; n=",nrow(temp.d),")",sep=""),
				DSS.XLAB,
				DSS.YLAB,
				line.name, # line names
				line.color=line.color,
				# none = no test shown
				single.test.type=single.test.type, # the test to show if specified show.test="single". 
				# the possible choices are logrank, wilcoxon, taroneware, all
				obs.survyrs=3,
				... 
			)
		},
		rfs={
			rfs.stats <- plot_km(temp.d,
				as.formula(paste(surv.formula.prefix,var.name)),
				paste(var.description,"(RFS; n=",nrow(temp.d),")",sep=""),
				RFS.XLAB,
				RFS.YLAB,
				line.name, # line names
				line.color=line.color,
				# none = no test shown
				single.test.type=single.test.type, # the test to show if specified show.test="single". 
				# the possible choices are logrank, wilcoxon, taroneware, all
				obs.survyrs=3,
				... 
			)
		}
	)
	
}


# not sure if PlotKM2 is ever used ... 
PlotKM2<-function(Survyrs,Survsts,class,ttl){
  gcols=c(CLASS.COLOR.POLE, CLASS.COLOR.MSI, CLASS.COLOR.CNLOW, CLASS.COLOR.CNHIGH)
  d=data.frame(Survyrs,Survsts,class);colnames(d)=c("years","status","class")
  d=d[complete.cases(d),]
  S=Surv(d$years,d$status)
 
  p=survdiff(S~d$class)
  fit<-survfit(S~d$class)
  plot(fit, main=ttl,lwd=3,col=gcols,xlim=c(0,8))
  legend("bottomright",levels(factor(class)),text.col=gcols, cex=0.8,box.lwd=0)
  text(3,0.25,paste("LR p-value: ",round(1 - pchisq(p$chisq,length(p$n) - 1),4), sep=""),cex=1)
}

####################################################
# function to do subtype x clinical classifier plot
#
# x - e.g. aes(TCGAfull[,"SUBTYPE.SIMPLE.NAME"])
# fill - e.g. aes(fill = TCGAfull[,"ESMO.RISK.GROUP"])
# file.name - file to write to, if NA, don't write anything
#
do.subtype.x.clin.risk.group.plot <- function(input.d, x, fill, title, file.name=NA) {
	if (!is.na(file.name)) {
		if (endsWith(file.name,"jpeg")) {
			jpeg(file.name)
		} else if (endsWith(file.name,"png")) {
			png(file.name)
		} else if (endsWith(file.name,"emf")) {
			win.metafile(file.name)
		} else if (endsWith(file.name,"pdf")) {
			pdf(file.name)
		}
	}
	format=theme(axis.title.x=element_text(size=15),legend.title=element_text(size=13),legend.text=element_text(size=13))
	plot.obj <- ggplot(
		input.d, 
		x
	) + geom_bar(fill,position='fill') +
	scale_fill_manual(name="Clinical\nRisk Groups",values = (brewer.pal(3, "Purples"))) + 
	ggtitle(title) +
	ylab("") + 
	xlab("") +
	format
	if(!is.na(file.name)) {
		dev.off()
	}
	return(plot.obj)
}
