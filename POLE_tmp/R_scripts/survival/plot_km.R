#################################
# some functions to do KM plots
#
# require getPval.R
#################################
source('getPval.R')
library(survival)

# helper function to find if a string ends with some string
endsWith <- function(a,b) {
	if (length(grep(b,a))==0) {
		return(FALSE) # not even substring ... must be false
	} else {
		return(substr(a,nchar(a)-nchar(b)+1,nchar(a))==b)
	}
}

##############################################
# some constants for the survival plot
#
BCSS.TITLE <- "Breast cancer specific survival"
BCSS.XLAB  <- "Total follow-up (years)"
BCSS.YLAB  <- "Cumulative breast cancer specific survival (BCSS)"

DSS.TITLE <- "Disease specific survival (DSS)"
DSS.XLAB  <- BCSS.XLAB
DSS.YLAB  <- DSS.TITLE

OS.TITLE <- "Overall survival"
OS.XLAB <- DSS.XLAB
OS.YLAB <- OS.TITLE

RFS.TITLE <- "Any relapse-free survival"
RFS.XLAB <- paste(RFS.TITLE,"time")
RFS.YLAB <- RFS.TITLE

DRFS.TITLE <- "Distant relapse-free survival"
DRFS.XLAB <- paste(DRFS.TITLE,"time")
DRFS.YLAB <- DRFS.TITLE

LRFS.TITLE <- "Rocal relapse-free survival"
LRFS.XLAB <- paste(LRFS.TITLE,"time")
LRFS.YLAB <- LRFS.TITLE

RRFS.TITLE <- "regional relapse-free survival"
RRFS.XLAB <- paste(RRFS.TITLE,"time")
RRFS.YLAB <- RRFS.TITLE

LRRFS.TITLE <- "Locoregional relapse-free survival"
LRRFS.XLAB <- paste(LRRFS.TITLE,"time")
LRRFS.YLAB <- LRRFS.TITLE
#
# end of constants
###############################################


#############################################
# KM plots with details of event counts
# - a more simple function to use SEE BELOW
#############################################
plot_km_detail <- function(input.data,
                           surv.formula,
                           main.text, 
                           xlab.text, 
                           ylab.text,
                           line.name,
                           ten.years.surv.95CI,
                           event.count,
                           line.color,
						   obs.survyrs,
						   line.pattern=NULL,
						   line.width=NULL,
						   legend.pos="bottomleft", # legend position keyword
                           file.name="no.file",
						   file.width=7,
						   file.height=7,
                           show.test="single", # show single or the reference group value (for pairwise comparison)
						   single.test.type="logrank", # the test to show if specified show.test="single". 
						                               # the possible choices are logrank, wilcoxon, taroneware, all
		                   round.digits.p.value, # number of digits for p-value
				   		   grey.scale=FALSE,
						   show.single.test.pos,...
                          ) {

   var.name <- deparse(surv.formula[[3]]) # this should be the biomarker name
                                          # the deparse() function is used to make sure var.name is a string
		
   #print(var.name)
   log.rank.p.values    <- c() # p-values to be returned
   wilcox.p.values      <- c() # p-values to be returned 		
   tarone.ware.p.values <- c() # p-values to be returned
   
   fit <- survfit(surv.formula, data=input.data)
   if (file.name != "no.file") {
      # do not generate a file if "no.file" is specified
	  file.name.len <- nchar(file.name)
	  if (file.name.len > 4) {
		file.name.extension <- tolower(substr(file.name,file.name.len-2,file.name.len))
		if (file.name.extension == "pdf") {
			#pdf(file=file.name)
			cairo_pdf(file=file.name, width=file.width, height=file.height) # good for unicode character in e.g. line.name
		} else if (file.name.extension %in% c("wmf","emf","wmz","emz")) {
			win.metafile(file=file.name, width=file.width, height=file.height)	
		} else if (file.name.extension %in% c("tif")) { # does not with with tiff since only check last three character!!!
			tiff(file=file.name, width=file.width*100, height=file.height*100)
		} else {
			# unknown extension ... do nothing
			file.name <- "no.file"
		}
      }
   } 
   which.strata.have.cases <- table(input.data[,var.name])>0 # in case some strata do not have any cases
   if (grey.scale) {
	   # gray scale plot
	   if (is.null(line.pattern)) {
		   line.pattern <- c(1:length(line.name))[which.strata.have.cases]
	   }
	   if (is.null(line.width)) {
		   line.width <- 1
	   }
	   plot(fit,
        	lty = line.pattern,  
			lwd=line.width,
        	main=main.text,
        	xlab=xlab.text,
        	ylab=ylab.text,...
    	)
		if (legend.pos=="top") {
			l1 <- legend(x=(max(fit$time,na.rm=TRUE)-min(fit$time,na.rm=TRUE))/2, y=0.99, # i.e. top 1% ... since survival plot always start at 100% survival
				legend=line.name,
				lty=line.pattern,
				lwd=line.width,
				box.lty=0,
				cex=0.8
			)
		} else {
			l1 <- legend(legend.pos,
				legend=line.name,
				lty=line.pattern,
				lwd=line.width,
				box.lty=0,
				cex=0.8
			)
		}
   } else {
	   # color plot
	   if (is.null(line.pattern)) {
	      line.pattern <- 1
	   }
	   if (is.null(line.width)) {
		  line.width <- 1
	   }
	   plot(fit,
			   col=line.color[which.strata.have.cases],
			   lty=line.pattern,
			   lwd=line.width,
			   main=main.text,
			   xlab=xlab.text,
			   ylab=ylab.text,...
	   )
	   if (legend.pos=="top") {
		   l1 <- legend(x=(max(fit$time,na.rm=TRUE)-min(fit$time,na.rm=TRUE))/2, y=0.99, # i.e. top 1% ... since survival plot always start at 100% survival
				   legend=line.name,
				   lty=line.pattern,
				   lwd=line.width,
				   col=line.color,
				   box.lty=0,
				   cex=0.8
		   )   
	   } else {
	   	l1 <- legend(legend.pos,
			   legend=line.name,
			   lty=line.pattern,
			   lwd=line.width,
			   col=line.color,
			   box.lty=0,
			   cex=0.8
	   	)
   	   }
   }
   # there seems to be need for the y-axis adjustment depending on the file.height ...
   dy <- 0.02 * (file.height-7)/(12-7) # determined empirically
   y.pos <- l1$rect$h-dy
   if (legend.pos=="top") {
	y.pos <- l1$rect$top+dy	   
   }
   l2 <- legend(
		   x=l1$rect$w+l1$rect$left,
		   y=y.pos,
		   legend=ten.years.surv.95CI,
		   title=paste(obs.survyrs,"yr 95% CI",sep=""),
		   title.col=1,
		   box.lty=0,
		   cex=0.8
   )
   l3 <- legend(
		   x=l1$rect$w+l1$rect$left + l2$rect$w,
		   y=y.pos,#l1$rect$h-dy,
           legend=event.count,
           title="Events/N",
           title.col=1,
           box.lty=0,
           cex=0.8
    )
	box()
   if (show.test == "single") {
      log.rank.test     <- survdiff(surv.formula, data=input.data, rho=0)
	  gehan.wilcox.test <- survdiff(surv.formula, data=input.data, rho=1)
	  tarone.ware.test  <- survdiff(surv.formula, data=input.data, rho=0.5) # http://courses.nus.edu.sg/course/stacar/internet/st3242/handouts/notes2.pdf
	  p.value <- getPval(log.rank.test); log.rank.p.values <- p.value
	  p.value <- round(p.value,digits=round.digits.p.value)
	  gehan.wilcox.p.value <- getPval(gehan.wilcox.test); wilcox.p.values <- gehan.wilcox.p.value
	  gehan.wilcox.p.value <- round(gehan.wilcox.p.value,digits=round.digits.p.value)
	  tarone.ware.p.value <- getPval(tarone.ware.test); tarone.ware.p.values <- tarone.ware.p.value
	  tarone.ware.p.value <- round(tarone.ware.p.value,digits=round.digits.p.value)
      text(
           x=l1$rect$w+l1$rect$left + l2$rect$w + 1.3*l3$rect$w,
		   y=show.single.test.pos, # position of the test statistics on plot
           paste(
				 ifelse(sum(single.test.type %in% c("logrank",   "all"))>=1,paste("Log-Rank p=",   sprintf(paste("%.",round.digits.p.value,"f",sep=""),p.value             ),"\n",sep=""),""),
				 ifelse(sum(single.test.type %in% c("wilcoxon",  "all"))>=1,paste("Wilcoxon p=",   sprintf(paste("%.",round.digits.p.value,"f",sep=""),gehan.wilcox.p.value),"\n",sep=""),""),
				 ifelse(sum(single.test.type %in% c("taroneware","all"))>=1,paste("Tarone-Ware p=",sprintf(paste("%.",round.digits.p.value,"f",sep=""),tarone.ware.p.value ),"\n",sep=""),""),
                 sep=""
                ),
           adj = c(0,0),
           cex=0.8
          )
   } else if (show.test != "none") {
      # assume show.test shows the reference group index
      legend.txt <- c()
	  value.names <- names(table(input.data[,var.name]))
      for (value.name in value.names) {
		 if (value.name == show.test) {
			 # this is the reference group
			legend.txt <- c(legend.txt, "reference group")
		 } else {
            # construct data
            temp.d <- input.data[input.data[,var.name] == show.test |
                              input.data[,var.name] == value.name,]
			if(sum(input.data[,var.name] == value.name,na.rm=TRUE)==0) {
				# no case in this group
				p.value <- NA 
				w.p.value <- NA
				t.p.value <- NA
			} else {
            # calculate log rank p-values
				p.value   <- getPval(survdiff(surv.formula, data=temp.d, rho=0 )); log.rank.p.values    <- c(log.rank.p.values,    p.value);  p.value   <- round(p.value,  digits=round.digits.p.value)
				w.p.value <- getPval(survdiff(surv.formula, data=temp.d, rho=1 )); wilcox.p.values      <- c(wilcox.p.values,      w.p.value);w.p.value <- round(w.p.value,digits=round.digits.p.value)
				t.p.value <- getPval(survdiff(surv.formula, data=temp.d, rho=0.5)); tarone.ware.p.values <- c(tarone.ware.p.values, t.p.value);t.p.value <- round(t.p.value,digits=round.digits.p.value)
			}
			new.txt <- paste(
					ifelse("logrank"    %in% single.test.type, paste(p.value,  " / ", sep=""),""),
					ifelse("wilcoxon"   %in% single.test.type, paste(w.p.value," / ", sep=""),""),
					ifelse("taroneware" %in% single.test.type,       t.p.value,     ""),
				sep=""
			)
			if (endsWith(new.txt," / ")) {new.txt <- substr(new.txt,0,nchar(new.txt)-3)}
            legend.txt <- c(legend.txt, new.txt)
	     }
      }

	  legend.title <- paste(
			  ifelse("logrank"    %in% single.test.type, "Log-Rank / ", ""),
			  ifelse("wilcoxon"   %in% single.test.type, "Wilcoxon / ", ""),
			  ifelse("taroneware" %in% single.test.type, "Tarone-Ware ",""),
			  sep="")
	  if (endsWith(legend.title," / ")) {legend.title <- substr(legend.title,0,nchar(legend.title)-2)}
	  legend.title <- paste(legend.title,"P-values",sep="")
	  
      l4 <- legend(x=l1$rect$w + l2$rect$w + l3$rect$w, y=y.pos,#y=l1$rect$h,
                legend=legend.txt,
                #text.col=line.color,
                title=legend.title,
                title.col=1,
                box.lty=0,
                cex=0.8
               )
   }
   if (file.name != "no.file") {
      # do not generate a file if "no.file" is specified
      dev.off()
   }
   return(list(
		"log.rank.p.values"=log.rank.p.values,
		"wilcox.p.values"=wilcox.p.values
	))	
}


###################################################
# a more simple function to call to draw KM plot
###################################################
plot_km <- function(input.d,
                    input.formula,
                    main.text,
                    xlab.text,
                    ylab.text,
                    line.name,
                    line.color,
					line.pattern=NULL,
					line.width=NULL,
                    show.test="single",   # show single or the reference group value (for pairwise comparison)
					                      # none = no test shown
					single.test.type="logrank", # the test to show if specified show.test="single". 
												# the possible choices are logrank, wilcoxon, taroneware, all
		            round.digits.p.value=4, # number of digits for p-value
		            obs.survyrs=10, # show the obs.survyrs (e.g. 10) survial rate on KM plot
					legend.pos="bottomleft", # legend position keyword
					file.name="no.file",
					file.width=7,
					file.height=7,
					grey.scale=FALSE,
					show.single.test.pos="default", # default: 0.5 if legend.pos="top" otherwise 0.1
					...) {

   # calculate "obs.survyrs"-yrs survival
   summary.surv.fit <- summary(survfit(input.formula, data=input.d), time=obs.survyrs, extend=T)

   n.cases <- table(input.d[,deparse(input.formula[[3]])]) # number of cases in each group variable = deparse(input.formula[[3]])
   decrement.count <- 0 # the number we need to take away from "i" because if n.cases[i]==0, length(summary.surv.fit) < length(line.name)
   
   ten.yrs.surv <- NULL
   for (i in 1:length(line.name)) {
		if (n.cases[i]==0) {
			# there must be no cases in this category
			ten.yrs.surv <- c(ten.yrs.surv,"NA")
			decrement.count <- decrement.count+1
		} else {
			ten.yrs.surv <- c(ten.yrs.surv,
                        paste(format(summary.surv.fit$surv*100, digits=3)[i-decrement.count],
                           "% (",
                           format(summary.surv.fit$lower*100, digits=3)[i-decrement.count],
                           "% - ",
                           format(summary.surv.fit$upper*100, digits=3)[i-decrement.count],
                           "%)", 
                           sep="")
                       )
		}
   }                   

   # summary of survival object to end of followup
   fit.obj <- survfit(input.formula, data=input.d)		
   summary.surv.fit.all <- summary(fit.obj)[['table']]

   # NEED TO RESET decrement.count!!!!
   decrement.count <- 0 # the number we need to take away from "i" because if n.cases[i]==0, length(summary.surv.fit) < length(line.name)
   
   event.count <- NULL
   for (i in 1:length(line.name)) {
	   if (n.cases[i]==0) {
		   # there must be no cases in this category
			event.count <- c(event.count,"NA")
			decrement.count <- decrement.count+1
	   } else {
	      event.count <- c(event.count,
                       paste(summary.surv.fit.all[i-decrement.count,'events'],
                          "/",
                          summary.surv.fit.all[i-decrement.count,'records'],
                          sep="")
                      )
		}
   }

   # determine show.single.test.pos
	if (show.single.test.pos=="default") {
		show.single.test.pos <- 0.1
		if (legend.pos=="top") {
			show.single.test.pos <- 0.5
		}
	}
   # plot km
   output <- plot_km_detail(input.d,
                  input.formula,
                  main.text, 
                  xlab.text, 
                  ylab.text,
                  line.name,
                  ten.yrs.surv,
                  event.count,
                  line.color,
				  obs.survyrs,
				  line.pattern=line.pattern,
				  line.width=line.width,
				  legend.pos=legend.pos,
                  file.name=file.name,
				  file.width=file.width,
				  file.height=file.height,
                  show.test=show.test,
				  round.digits.p.value=round.digits.p.value,
				  single.test.type=single.test.type,
				  grey.scale=grey.scale,
				  show.single.test.pos=show.single.test.pos,...
                 )
	return(list(
		"log.rank.p.values"=output$log.rank.p.values,
		"wilcox.p.values"=output$wilcox.p.values,
		"n"=sum(fit.obj$n),
		"nevent"=sum(fit.obj$n.event)
	))
}



############################################################################
# try to find single cut point by maximizing hazard ratio or minimzing AIC
# 
# WARNING!!! surv.formula CANNOT BE MULTIVARIABLE!!!!
#            i.e. "Surv(time, status) ~ x + age" ... DOES NOT WORK!!!
#                 "Surv(time, status) ~ x"  ... ok
# returns NA if
#    - surv.formula is not univariable
#    - variable is not numeric
#    - no variation on the variable of interest
#
find.cut.point.by.coxph <- function(input.d,surv.formula){
	# make sure is univariable formula 
	if (length(surv.formula[[3]]) > 1) {
		print(paste("ERROR (find.cut.point.by.coxph): unable to process the survival formula: ",deparse(surv.formula)))
		print("NOTE: formula must be of form: Surv(time,status) ~ variable")
		return(NA)
	}
	
	# find variable name
	var.name <- deparse(surv.formula[[3]])
	
	if (!is.numeric(input.d[,var.name])) {
		print(paste("ERROR (find.cut.point.by.coxph): variable (",var.name,") is NOT NUMERIC!!!"))
		return(NA)
	}
	
	# find the min,max value of the variable
	possible.values <- as.numeric(names(table(input.d[,var.name])))
	num.possible.values <- length(possible.values)
	if (num.possible.values < 2) {
		print(paste("ERROR (find.cut.point.by.coxph): variable (",var.name,") do not have any variation (i.e. all same value) for cut point determination."))
		return(NA)
	}
	
	# try all possible cut point
	cut.points <- c()
	aics <- c()
	hrs <- c()
	for (cut.point in possible.values[2:length(possible.values)]) {
		cox.model.fit <- NA
		tryCatch({
					cox.model.fit <- coxph(as.formula(paste(deparse(surv.formula),">=",cut.point)),data=input.d)
				}, warning = function(w) {
					# just ignore silently
				}, error = function(e) {
					print(paste("ERROR (find.cut.point.by.coxph) unexpected error occured:",e))
				}, finally = function(e) {
					# just ingore silently
				})
		if (length(cox.model.fit) > 1) { # cox.model.fit must not be NA, otherwise, length(cox.model.fit) would be 1
			# cox model with no warning/error
			cut.points <- c(cut.points, cut.point)
			aics       <- c(aics,       extractAIC(cox.model.fit)[2])
			hrs        <- c(hrs,        exp(cox.model.fit$coefficients[[1]]))
		}
	}
	
	# return results
	return(list(
					"best.cut.point.by.aic"=cut.points[which.min(aics)],
					"best.cut.point.by.hr"=cut.points[which.max(hrs)],
					"cut.points"=cut.points,
					"aics"=aics,
					"hazard.ratios"=hrs
			))
}

