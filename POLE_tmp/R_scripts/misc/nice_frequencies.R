# Some functions to pretty pretty tables of frequencies
# 
# TODO: try htmlTable (http://cran.r-project.org/web/packages/htmlTable/vignettes/tables.html)
#
# Author: sleung
###############################################################################
source("misc_helpers.R")
require(caret)

##############################################################
# generate summary table as html table 
# assume d is an array of numbers
summary.as.html <- function(d) {
	col.th.style <- COL.TH.STYLE
	s.table <- summary(d) # summary table
	result <- "<table>"
	result <- paste(result,"<tr><th style='",col.th.style,"'>",paste(names(s.table),collapse=paste("</th><th style='",col.th.style,"'>",sep="")),"</th></tr>",sep="")
	result <- paste(result,"<tr><td>",paste(s.table,collapse="</td><td>"),"</td></tr>",sep="")
	result <- paste(result,"</table>",sep="")
	return(result)
}

####################################################
# same as row.col.percent but show table in html
row.col.percent.as.html <- function(
		t, 
		show.count=FALSE, # whether to show the count
		#pretty.text=FALSE, digits=4, # these are part of ... i.e. argment for row/col.percent
		row.names=NULL,
		column.names=NULL,
		html.table.border=0,
		banded.rows=FALSE,
		css.class.name.odd="odd",
		css.class.name.even="even",
		...
	) {
		col.th.style <- COL.TH.STYLE
		row.th.style <- ROW.TH.STYLE
		table.values <- row.col.percent(t,...)
		result <- paste("<table border=",html.table.border,">",sep="")
		# print header
		if (!is.null(column.names)) {
			colnames(table.values) <- column.names
		} else {
			column.names <- colnames(table.values)
		}
		if (!is.null(row.names)) {
			rownames(table.values)[rep(c(TRUE,FALSE,FALSE),nrow(table.values)/3)] <- row.names
		} else {
			row.names <- rownames(table.values)[rep(c(TRUE,FALSE,FALSE),nrow(table.values)/3)]
		}
		result <- paste(result,"<tr><th style='",col.th.style,"' colspan=2></th><th style='",col.th.style,"'>",paste(column.names,collapse=paste("</th><th style='",col.th.style,"'>",sep="")),"</th></tr>",sep="")
		# print values
		i <- 1
		num.row.per.cat <- ifelse(show.count,3,2)
		while(i<=nrow(table.values)) {
			tr.class <- ifelse(banded.rows,paste(" class='",ifelse((floor(i/3)+1)%%2==0,css.class.name.even,css.class.name.odd),"'",sep=""),"")
			result <- paste(result,"<tr",tr.class,"><th style='",row.th.style,"' rowspan=",num.row.per.cat,">",row.names[floor(i/3)+1],"</th>",sep="")
			if (show.count) {
				result <- paste(result,"<th style='",row.th.style,"'>count</th><td>",paste(table.values[i,],collapse="</td><td>"),"</td></tr>",sep="")	
			}
			i<-i+1
			if (show.count) {
				result <- paste(result,"<tr",tr.class,"><th style='",row.th.style,"'><i>row %</i></th><td><i>",paste(table.values[i,],collapse="</i></td><td><i>"),"</td></tr>",sep="")
			} else {
				result <- paste(result,"<tr",tr.class,"><th style='",row.th.style,"'>row %</th><td>",paste(table.values[i,],collapse="</td><td>"),"</td></tr>",sep="")
			}
			i<-i+1
			if (show.count){
				result <- paste(result,"<tr",tr.class,"><th style='",row.th.style,"'><i>col %</i></th><td><i>",paste(table.values[i,],collapse="</i></td><td><i>"),"</td></tr>",sep="")
			} else {
				result <- paste(result,"<tr",tr.class,"><th style='",row.th.style,"'>col %</th><td>",paste(table.values[i,],collapse="</td><td>"),"</td></tr>",sep="")
			}
			i<-i+1
		}
		result <- paste(result,"</table>",sep="")
		return(result)
}


#########################################
# helper function to pring results from
# confusionMatrix as a html table
#
# num.boot.for.ci: number of bootstrap interval for CI.
# if boot.for.ci=NA, do not show CI
confusionResultToHtmlTable <- function(prediction, reference, ref.description, round.digits.p.value, num.boot.for.ci=NA, seed=12) {
	td.right <- "<td style='text-align: right; white-space: nowrap;'>"
	td.left <- "<td style='text-align: left; white-space: nowrap;'>"

	# need to set prediction and reference as factors
	prediction <- as.factor(prediction)
	reference <-as.factor(reference)
	
	confusionResult <- confusionMatrix(prediction,reference)
	
	multi.levels <- !is.null(nrow(confusionResult$byClass)) # i.e. class with > 2 levels e.g. 3x3 table
	if (multi.levels) {
		confusionResult$byClass <- apply(confusionResult$byClass,2,mean)
	}
	
	do.ci <- !is.na(num.boot.for.ci)
	boot.confusionResults <- NA
	if (!is.na(num.boot.for.ci)) {
		# do bootstrap confidence interval!!!
		boot.confusionResults <- as.data.frame(cbind(
			"kappa"=rep(NA,num.boot.for.ci),
			"sens"=rep(NA,num.boot.for.ci),
			"specs"=rep(NA,num.boot.for.ci),
			"ppv"=rep(NA,num.boot.for.ci),
			"npv"=rep(NA,num.boot.for.ci)
		))
		set.seed(seed)
		for (i in 1:num.boot.for.ci) {
			boot.indexes <- sample(1:length(prediction),replace=TRUE)
			#print(table(prediction[boot.indexes]))
			#print(table(reference[boot.indexes]))
			boot.confusionResult <- confusionMatrix(prediction[boot.indexes],reference[boot.indexes]) 
			boot.confusionResults$kappa[i] <- boot.confusionResult$overall[["Kappa"]]
			if (multi.levels) {
				boot.confusionResult$byClass <- apply(boot.confusionResult$byClass,2,mean)
			}
			boot.confusionResults$sens[i]  <- boot.confusionResult$byClass["Sensitivity"]
			boot.confusionResults$specs[i] <- boot.confusionResult$byClass["Specificity"]
			boot.confusionResults$ppv[i]   <- boot.confusionResult$byClass["Pos Pred Value"]
			boot.confusionResults$npv[i]   <- boot.confusionResult$byClass["Neg Pred Value"]
		}
		boot.kappas <- sort(boot.confusionResults$kappa); boot.kappas <- boot.kappas[!is.na(boot.kappas)]
		boot.sens   <- sort(boot.confusionResults$sens);  boot.sens   <- boot.sens[  !is.na(boot.sens)]
		boot.specs  <- sort(boot.confusionResults$specs); boot.specs  <- boot.specs[ !is.na(boot.specs)]
		boot.ppvs   <- sort(boot.confusionResults$ppv);   boot.ppvs   <- boot.ppvs[  !is.na(boot.ppvs)]		
		boot.npvs   <- sort(boot.confusionResults$npv);   boot.npvs   <- boot.npvs[  !is.na(boot.npvs)]	
		kappa.ci <- boot.kappas[round(c(0.025, 0.975)*length(boot.kappas))]
		sens.ci  <- boot.sens[  round(c(0.025, 0.975)*length(boot.sens))]
		specs.ci <- boot.specs[ round(c(0.025, 0.975)*length(boot.specs))]
		ppv.ci   <- boot.ppvs[  round(c(0.025, 0.975)*length(boot.ppvs))]
		npv.ci   <- boot.npvs[  round(c(0.025, 0.975)*length(boot.npvs))]
	}
	
	return(paste(
		"<table>",
		"<tr>",td.right,"Reference:</td>",td.left,ref.description,
		"</td></tr>",
		"<tr>",td.right,"Accuracy (95%CI):</td>",td.left,
		round(confusionResult$overall[["Accuracy"]],digits=round.digits.p.value)," (",round(confusionResult$overall[["AccuracyLower"]],digits=round.digits.p.value)," - ",round(confusionResult$overall[["AccuracyUpper"]],digits=round.digits.p.value),")",
		"</td></tr>",
		"<tr>",td.right,"No Information Rate:</td>",td.left,
		round(confusionResult$overall[["AccuracyNull"]],digits=round.digits.p.value),
		"</td></tr>",
		"<tr>",td.right,"P-Value [Acc > NIR]:</td>",td.left,
		round(confusionResult$overall[["AccuracyPValue"]],digits=round.digits.p.value),
		"</td></tr>",
							
		"<tr><td> </td><td><td></tr>",
									
		"<tr>",td.right,"Kappa",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
		round(confusionResult$overall[["Kappa"]],digits=round.digits.p.value),
		ifelse(
			do.ci,
			paste(" (",round(kappa.ci[1],digits=round.digits.p.value),"-",round(kappa.ci[2],digits=round.digits.p.value),")",sep=""),
			""
		),
		"</td></tr>",
		"<tr>",td.right,"Mcnemar's Test P-Value:</td>",td.left,
		round(confusionResult$overall[["McnemarPValue"]],digits=round.digits.p.value),
		"</td></tr>",
									
		"<tr>",
		ifelse(!multi.levels ,"<td> </td><td><td>","<td style='text-align: left' colspan=2><i>note: the following are mean values across classes</i></td>"),
		"</tr>",
		
		"<tr>",td.right,"Sensitivity",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
		round(confusionResult$byClass["Sensitivity"],digits=round.digits.p.value),
		ifelse(
			do.ci,
			paste(" (",round(sens.ci[1],digits=round.digits.p.value),"-",round(sens.ci[2],digits=round.digits.p.value),")",sep=""),
			""
		),
		"</td></tr>",
		"<tr>",td.right,"Specificity",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
		round(confusionResult$byClass["Specificity"],digits=round.digits.p.value),
		ifelse(
			do.ci,
			paste(" (",round(specs.ci[1],digits=round.digits.p.value),"-",round(specs.ci[2],digits=round.digits.p.value),")",sep=""),
			""
		),
		"</td></tr>",
		"<tr>",td.right,"PPV",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
		round(confusionResult$byClass["Pos Pred Value"],digits=round.digits.p.value),
		ifelse(
			do.ci,
			paste(" (",round(ppv.ci[1],digits=round.digits.p.value),"-",round(ppv.ci[2],digits=round.digits.p.value),")",sep=""),
			""
		),
		"</td></tr>",
		"<tr>",td.right,"NPV",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
		round(confusionResult$byClass["Neg Pred Value"],digits=round.digits.p.value),
		ifelse(
			do.ci,
			paste(" (",round(npv.ci[1],digits=round.digits.p.value),"-",round(npv.ci[2],digits=round.digits.p.value),")",sep=""),
			""
		),
		"</td></tr>",
		"<tr>",td.right,"Prevalence:</td>",td.left,
		round(confusionResult$byClass["Prevalence"],digits=round.digits.p.value),
		"</td></tr>",
		"<tr>",td.right,"Detection Rate:</td>",td.left,
		round(confusionResult$byClass["Detection Rate"],digits=round.digits.p.value),
		"</td></tr>",
		"<tr>",td.right,"Detection Prevalence:</td>",td.left,
		round(confusionResult$byClass["Detection Prevalence"],digits=round.digits.p.value),
		"</td></tr>",
		"<tr>",td.right,"Balanced Accuracy:</td>",td.left,
		round(confusionResult$byClass["Balanced Accuracy"],digits=round.digits.p.value),
		"</td></tr>",
					
		"</table>",
		sep=""
	))
}

####################################################
# return cohort characteristics table
#
# input.d - data matrix
# marker.name - marker of interest i.e. marker being studies
# marker.description - description of the marker of interest to be shown on the result table
# var.names - names of clinicopathological parameters
# is.var.continuous - array of TRUE/FALSE to indicate whether the variable is continuous
# var.description - a description of the variables to be shown on the result table
# show.missing - whether to show the missing cases or now
# show.percent - whether to show column, row or both percent.  Note: for the total column, always show column percent
# stat.tests - a list of statistic tests to perform ... NULL indicate do not do test for all variables, NA indicate do not do test for specified variable.
#            - test: chisq, fisher, ttest, wilcox, kendall, spearman, pearson, kruskal, confusionMarkerAsRef, confusionVarAsRef
# round.digits.p.value - digits to round off for p-values
# missing.codes.highlight - a list, defining the categories to be shown as missing i.e. distinct counts for these categories and no percentages will be calculated
# missing.codes - coding for missing values
# decimal - decimal places to show
#
# marker of interest MUSH BE categorical!!! i.e. cannot be continuous
do.cohort.characteristics <- function(
		input.d, 
		marker.name, 
		marker.description, 
		var.names, 
		is.var.continuous, 
		var.descriptions, 
		marker.value.labels.tolower=TRUE, # put marker value labels to lower case
		show.missing=TRUE,
		show.missing.continuous=TRUE, # if set to false and show.missing==FALSE, do not show # of missing cases for continuous variables, 
		                              # otherwise, show # of missing for continuous variables even if show.missing==FALSE.
		do.droplevels=TRUE, # drop categories of unobserved categories
		show.percent="both", # other possible values: "column", "row"
		stat.tests=NULL,
		stat.test.column.header="association/correlation test", # the column name of the stat test column in the html table
		round.digits.p.value=4,
		num.boot.for.ci=1000, # number of bootstrap samples for any bootstrap method that may be used
		missing.codes.highlight=NULL, # missing categories that want to show, i.e. instead of lumping with other missing values ... this is a 2-d matrix with columns=variable
		missing.codes=c("N/A","","Unk"),
		decimal=0,
		caption=NA, # caption for table
		html.table.border=0,
		banded.rows=FALSE,
		css.class.name.odd="odd",
		css.class.name.even="even"
) {
	col.th.style <- COL.TH.STYLE
	row.th.style <- "text-align: left; padding-right:10px; padding-right:10px;"
	
	if (is.null(missing.codes.highlight)) {
		missing.codes.highlight <- list()
		for (var.name in var.names) {
			missing.codes.highlight[var.name] <- NA
		}
	}
	
	### input.d.no.missing DEFINED HERE!!!
	input.d.no.missing <- input.d[!is.na(input.d[,marker.name]) & !input.d[,marker.name] %in% missing.codes,] # no missing marker ... may still have missing for other parameters
	if (is.factor(input.d.no.missing[,marker.name]) & do.droplevels) {
		input.d.no.missing[,marker.name] <- droplevels(input.d.no.missing[,marker.name])
	}
	marker.categories <- names(table(input.d.no.missing[,marker.name]))
	marker.categories <- marker.categories[!marker.categories %in% missing.codes]
	total.label <- ifelse(sum(sapply(var.descriptions,function(x){return(ifelse(isFirstLetterUpperCase(x),1,0))}))==length(var.descriptions),"Total","total")
	if (marker.value.labels.tolower) {
		result.table.col.names <- c(total.label,paste(marker.description,tolower(marker.categories)))
	} else {
		result.table.col.names <- c(total.label,paste(marker.description,marker.categories))	
	}

	# if all items in var.descriptions starts with capital letter, first row header should be capital as well i.e. "Total"
	result.table.row.names <- total.label
	result.table <- c(
			paste(nrow(input.d)," (100%)",sep=""),
			sapply(marker.categories,function(x){
						return(paste(sum(input.d.no.missing[,marker.name]==x)," (",round(sum(input.d.no.missing[,marker.name]==x)/nrow(input.d.no.missing)*100,decimal),"%)",sep=""))})
	)
	do.stats <- !is.null(stat.tests)
	stat.tests.results <- c()
	
	num.var <- length(var.names)
	
	for (i in 1:num.var) {
		#############################
		# NOTE: three data matrices here ...
		# input.d.no.missing - no missing marker only (may contain missing var)
		# input.d.no.missing.var.only - missing var only (may contain missing marker)
		# input.d.no.missing.var - no missing marker and no missing var
		#############################
						
		# header row for each var
		result.table <- rbind(
				result.table,
				c("",rep("",length(marker.categories)))
		)
		result.table.row.names <- c(result.table.row.names,var.descriptions[i])
		var.name <- var.names[i]
		var.description <- var.descriptions[i]
		
		num.missing.row.header.name <- c()
		if (sum(!is.na(missing.codes.highlight[[var.name]]))>0) {
			num.missing.row.header.name <- c(num.missing.row.header.name, missing.codes.highlight[[var.name]]) # need double [[]] for missing.codes.highlight because its a list
		}
		num.missing.row.header.name <- c(num.missing.row.header.name,"missing")
		
		### input.d.no.missing.var.only DEFINED HERE!!!
		input.d.no.missing.var.only <- input.d[!is.na(input.d[,var.name]) & !input.d[,var.name]%in%missing.codes & !input.d[,var.name]%in%missing.codes.highlight[[var.name]],]
		if (is.factor(input.d.no.missing.var.only[,var.name]) & do.droplevels) {
			input.d.no.missing.var.only[,var.name] <- droplevels(input.d.no.missing.var.only[,var.name])
		}
		### input.d.no.missing.var DEFINED HERE!!!
		input.d.no.missing.var <- input.d.no.missing.var.only[!is.na(input.d.no.missing.var.only[,marker.name]) & !input.d.no.missing.var.only[,marker.name] %in% missing.codes,]
		if (is.factor(input.d.no.missing.var[,marker.name]) & do.droplevels) {
			input.d.no.missing.var[,marker.name] <- droplevels(input.d.no.missing.var[,marker.name])
		}
		if (is.var.continuous[i]) { # continuous variable
			input.d.no.missing.var.only[,var.name] <- as.numeric(input.d.no.missing.var.only[,var.name])
			input.d.no.missing.var[,     var.name] <- as.numeric(input.d.no.missing.var[,     var.name])
			# continuous variable - 4 rows: mean (+/- std dev) / median / IQR / number of missing
			var.row.names <- c("mean","median","interquartile range")
			if (show.missing | show.missing.continuous) {
				var.row.names <- c(var.row.names, num.missing.row.header.name)
			}
			result.table.row.names <- c(result.table.row.names,var.row.names)
			
			# do stat test for continuous variables (ignore non-applicable tests!) ...
			stat.test.result <- NA
			if (do.stats) {
				switch (stat.tests[i],
						spearman={
							spearman.result <- cor.test(input.d.no.missing.var[,var.name],as.numeric(input.d.no.missing.var[,marker.name]),method="spearman")
							stat.test.result <- paste(
									"Spearman correlation<br>",
									"rho = ",round(spearman.result$estimate,2),
									"<br>P = ",sprintf(paste("%.",round.digits.p.value,"f",sep=""),round(spearman.result$p.value,digits=round.digits.p.value)),sep="")
						},
						kruskal={
							kruskal.result <- kruskal.test(input.d.no.missing.var[,var.name] ~ as.numeric(input.d.no.missing.var[,marker.name]))
							stat.test.result <- paste(
									"Kruskal-Wallis rank sum test<br>",
									"<br>P = ",sprintf(paste("%.",round.digits.p.value,"f",sep=""),round(kruskal.result$p.value,digits=round.digits.p.value)),sep="")
						},
						wilcox={
							wilcox.result <- wilcox.test(input.d.no.missing.var[,var.name] ~ as.numeric(input.d.no.missing.var[,marker.name]))
							stat.test.result <- paste(
									"Wilcoxon rank sum test<br>",
									"P = ",sprintf(paste("%.",round.digits.p.value,"f",sep=""),round(wilcox.result$p.value,digits=round.digits.p.value)),
									sep=""
							)
						},
				)
				stat.tests.results <- c(stat.tests.results,stat.test.result)
			}
			
			result.table <- rbind(result.table,
					c( # mean
							paste(
								round(mean(input.d.no.missing.var.only[,var.name]),decimal),
								round(sem( input.d.no.missing.var.only[,var.name]),decimal),
								sep=" &#177; "), 
							sapply(marker.categories,function(x){
										temp.d <- input.d.no.missing.var[input.d.no.missing.var[,marker.name]==x,var.name]
										if (length(temp.d)==0){
											return(MISSING.EXPLICIT)
										} else {
											return(paste(
												round(mean(temp.d),decimal),
												round(sem( temp.d),decimal),											
												sep=" &#177; "
											))		
										}
									})
					),
					c( # median
							round(median(input.d.no.missing.var.only[,var.name]),decimal), 
							sapply(marker.categories,function(x){
										temp.d <- input.d.no.missing.var[input.d.no.missing.var[,marker.name]==x,var.name]
										if (length(temp.d)==0){
											return(MISSING.EXPLICIT)
										} else {
											return(round(median(temp.d),decimal))
										}
									})
					),
					c( # inter quartile range
							paste(round(quantile(input.d.no.missing.var.only[,var.name],c(0.25,0.75)),decimal),collapse=" to "), 
							sapply(marker.categories,function(x){
										temp.d <- input.d.no.missing.var[input.d.no.missing.var[,marker.name]==x,var.name]
										if (length(temp.d)==0){
											return(MISSING.EXPLICIT)
										} else {
											return(paste(round(quantile(temp.d,c(0.25,0.75)),decimal),collapse=" to "))		
										}
									})
					)
			)
		} else { # categorical variable
			var.categories <- names(table(input.d.no.missing.var.only[,var.name]))
			var.row.names <- var.categories
			if (show.missing) {
				var.row.names <- c(var.row.names, num.missing.row.header.name)
			}
			result.table.row.names <- c(result.table.row.names,var.row.names)
			
			# do stat test for continuous variables (ignore non-applicable tests!) ...
			stat.test.result <- NA
			if (do.stats) {
				switch (stat.tests[i],
						kendall={
							kendall.result <- cor.test(as.numeric(input.d.no.missing.var[,var.name]),as.numeric(input.d.no.missing.var[,marker.name]),method="kendall")
							stat.test.result <- paste(
								"Kendall correlation<br>",
								"tau = ",round(kendall.result$estimate,2),
								"<br>P = ",sprintf(paste("%.",round.digits.p.value,"f",sep=""),round(kendall.result$p.value,digits=round.digits.p.value)),sep="")
						},
						chisq={
							chisq.result <- chisq.test(table(
								input.d.no.missing.var[,var.name],
								input.d.no.missing.var[,marker.name]
							))
							stat.test.result <- paste(
								"Chi-square test<br>",
								"P = ",sprintf(paste("%.",round.digits.p.value,"f",sep=""),round(chisq.result$p.value,digits=round.digits.p.value)),
								sep=""
							)
						},
						fisher={
							fisher.result <- fisher.test(table(
								input.d.no.missing.var[,var.name],
								input.d.no.missing.var[,marker.name]
							),workspace=2e6)
							stat.test.result <- paste(
								"Fisher's exact test<br>",
								"P = ",sprintf(paste("%.",round.digits.p.value,"f",sep=""),round(fisher.result$p.value,digits=round.digits.p.value)),
								sep=""
							)
						},
						confusionMarkerAsRef={ # confusion matrix, marker as the reference
							# require both marker and var to be factor ...
							# if not, just print err msg
							if (!is.factor(input.d.no.missing.var[,var.name]) | !is.factor(input.d.no.missing.var[,marker.name])) {
								stat.test.result <- "error: both marker and variable needs to be factor"
							} else {
								stat.test.result <- confusionResultToHtmlTable(
									as.numeric(input.d.no.missing.var[,var.name]),as.numeric(input.d.no.missing.var[,marker.name]), 
									marker.description,
									round.digits.p.value,
									num.boot.for.ci=num.boot.for.ci
								)
							}
						},
						confusionVarAsRef={ # confusion matrix, variable as the reference
							# require both marker and var to be factor ...
							# if not, just print err msg
							if (!is.factor(input.d.no.missing.var[,var.name]) | !is.factor(input.d.no.missing.var[,marker.name])) {
								stat.test.result <- "error: both marker and variable needs to be factor"
							} else {
								stat.test.result <- confusionResultToHtmlTable(
									as.numeric(input.d.no.missing.var[,marker.name]),as.numeric(input.d.no.missing.var[,var.name]), 
									var.description,
									round.digits.p.value,
									num.boot.for.ci=num.boot.for.ci
								)
							}
						}
				)
				stat.tests.results <- c(stat.tests.results,stat.test.result)
			}
			
			for (var.category in var.categories) {
				total.value <- paste(
					sum(input.d.no.missing.var.only[,var.name]==var.category),
					" (",
					round(sum(input.d.no.missing.var.only[,var.name]==var.category)/nrow(input.d.no.missing.var.only)*100,decimal),
					"%)",
					sep=""
				)
				result.table <- rbind(result.table,
					switch(show.percent,
						row={c(
							total.value,
							sapply(marker.categories,function(x){
								return(paste(
									sum(input.d.no.missing.var[,var.name]==var.category & input.d.no.missing.var[,marker.name]==x),
									" (",
									ifelse(
										sum(input.d.no.missing.var[,var.name]==var.category) > 0,
										round(sum(input.d.no.missing.var[,var.name]==var.category & input.d.no.missing.var[,marker.name]==x)/sum(input.d.no.missing.var[,var.name]==var.category)*100,decimal),
										0
									),
									"%)",
									sep=""
								))
							})
						)},
					column={},
					both={c(
						total.value,
							sapply(marker.categories,function(x){
								return(paste(
									sum(input.d.no.missing.var[,var.name]==var.category & input.d.no.missing.var[,marker.name]==x),
									"<i><br>",
									ifelse(
										sum(input.d.no.missing.var[,var.name]==var.category) > 0,
										round(sum(input.d.no.missing.var[,var.name]==var.category & input.d.no.missing.var[,marker.name]==x)/sum(input.d.no.missing.var[,var.name]==var.category)*100,decimal),
										0
									),
									"%<br>",
									ifelse(
										sum(input.d.no.missing.var[,marker.name]==x) > 0,
										round(sum(input.d.no.missing.var[,var.name]==var.category & input.d.no.missing.var[,marker.name]==x)/sum(input.d.no.missing.var[,marker.name]==x)*100,decimal),
										0
									),
									"%</i>",
									sep=""
								))
							})
						)}
					)
				)
			}
		}
		if (show.missing | is.var.continuous[i]&show.missing.continuous) {
			if (sum(!is.na(missing.codes.highlight[[var.name]]))>0) {
				# there's some missing values we want to highlight ...
				for (missing.code in missing.codes.highlight[[var.name]]) {
					result.table <- rbind(result.table,
						c( # number of missing
							sum(!is.na(input.d[,var.name]) & input.d[,var.name]==missing.code),
							sapply(marker.categories,function(x){
								temp.d <- input.d.no.missing[input.d.no.missing[,marker.name]==x,var.name]
								return(sum(!is.na(temp.d) & temp.d==missing.code))		
							})
						)
					)
				}
			} 
			result.table <- rbind(result.table,
				c( # number of missing
					sum(is.na(input.d[,var.name]) | input.d[,var.name]%in%missing.codes),
					sapply(marker.categories,function(x){
						temp.d <- input.d.no.missing[input.d.no.missing[,marker.name]==x,var.name]
						return(sum(is.na(temp.d) | temp.d%in%missing.codes))		
					})
				)
			)
		}
	}
	colnames(result.table) <- result.table.col.names
	rownames(result.table) <- result.table.row.names

	# generate html table ...	
	result.table.html <- paste("<table border=",html.table.border,">",ifelse(is.na(caption),"",paste("<caption style='",TABLE.CAPTION.STYLE,"'>",add.table.number(caption),"</caption>",sep="")),sep="")
	result.table.html <- paste(
			result.table.html,
			"<tr><th style='",col.th.style,"' colspan=2></th><th style='",col.th.style,"'>",
			paste(
				result.table.col.names,collapse=paste("</th><th style='",col.th.style,"'>",
				sep="")
			),
			"</th>",
			ifelse(do.stats,paste("<th style='",col.th.style,"'>",stat.test.column.header,"</th>",sep=""),""),
			"</tr>",
			sep=""
	)
	
	row.band.toggle <- TRUE
	var.count <- -1 # want to skip the header row which contains the total count
	for (i in 1:nrow(result.table)) {
		
		num.missing.row.header.name <- c()
		if (var.count > 0) {
			var.name <- var.names[var.count]
			if (sum(!is.na(missing.codes.highlight[[var.name]]))>0) {
				num.missing.row.header.name <- c(num.missing.row.header.name, missing.codes.highlight[[var.name]]) # need double [[]] for missing.codes.highlight because its a list
			}
		}
		num.missing.row.header.name <- c(num.missing.row.header.name,"missing")
		
		
		# check to see if this is the start of a new category or 1st row ...
		var.header.row <- FALSE
		if ( i==1 | sum(result.table[i,]=="",na.rm=TRUE)>1) {
			# this must be the start of a first category since row contains >1 empty cells (there will be 1 empty cell for any var with no stats test)
			var.header.row <- TRUE
			var.count <- var.count+1
			if (i > 1) { # varname not available for i==1 (the row for the total)
				var.name <- var.names[var.count]
			}
			row.band.toggle <- !row.band.toggle
			tr.class <- ifelse(banded.rows,paste(" class='",ifelse(row.band.toggle,css.class.name.even,css.class.name.odd),"'",sep=""),"")
			result.table.html <- paste(
					result.table.html,
					"<tr",tr.class,"><th style='",row.th.style,"' colspan=",2,">",result.table.row.names[i],"</th>",
					sep=""
			)
		} else {
			if (num.var==1) {
				row.band.toggle <- !row.band.toggle # always toggle when there's only one variable
			}
			tr.class <- ifelse(banded.rows,paste(" class='",ifelse(row.band.toggle,css.class.name.even,css.class.name.odd),"'",sep=""),"")
			result.table.html <- paste(
					result.table.html,
					"<tr",tr.class,"><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='",row.th.style,"'>",
					result.table.row.names[i],
					ifelse(!is.var.continuous[var.count] & show.percent=="both",
						ifelse(!result.table.row.names[i]%in%num.missing.row.header.name | !show.missing,"<i><br>&nbsp;&nbsp;&nbsp;&nbsp;row%<br>&nbsp;&nbsp;&nbsp;&nbsp;col%</i>",""),
						""),
					"</th>",
					sep=""
			)
		}	 
		
		result.table.html <- paste(
			result.table.html,
			"<td>",paste(result.table[i,],collapse="</td><td>"),"</td>",
			ifelse(
				(var.header.row & do.stats & var.count>0) | i==1, 
				ifelse(i==1,
					"<td></td>",
					paste(
						"<td rowspan=",
						ifelse(
							is.var.continuous[var.count],
							ifelse(show.missing,1+sum(!is.na(missing.codes.highlight[[var.name]])),0)+4,
							ifelse(show.missing,1+sum(!is.na(missing.codes.highlight[[var.name]])),0)+1+as.numeric(ifelse(
								is.factor(input.d.no.missing[,var.name]),
								length(names(table(droplevels(input.d.no.missing.var.only[!is.na(input.d.no.missing.var.only[,var.name]) & !input.d.no.missing.var.only[,var.name]%in%c(missing.codes,missing.codes.highlight[[var.name]]),var.name])))),
								length(names(table(           input.d.no.missing.var.only[!is.na(input.d.no.missing.var.only[,var.name]) & !input.d.no.missing.var.only[,var.name]%in%c(missing.codes,missing.codes.highlight[[var.name]]),var.name] )))
							))
						),
						">",stat.tests.results[var.count],"</td>",
						sep=""
					)
				),
				""
			),
			"</tr>",sep="")
	}
	
	result.table.html <- paste(result.table.html,"</table>",sep="")
	return(list("result.table"=result.table, "stat.tests.results"=stat.tests.results, "result.table.html"=result.table.html))
}



#################################################################
# generate histogram, with median, Q1/3
# NOTE: expect missing to be NA!!! DO NOT filter out missing biomarker data
#       as this function reports missing data count
do.hist <- function(input.d, data.description, biomarker.var.name, biomarker.name, show.title=TRUE, br=100, digits=3,interpretable.label="scorable") {
	xlab.text <- biomarker.name
	biomarker <- input.d[,biomarker.var.name]
	quantile.output <- quantile(biomarker,na.rm=TRUE)
	data.description <- ifelse(is.na(data.description),"",paste(data.description,"\n",sep=""))
	hist(
		biomarker,
		br=br,
		main=ifelse(show.title,paste(
			data.description,
			"mean (min,Q1,median,Q3,max): ",
			format(mean(biomarker,na.rm=TRUE),digits=digits)," ",
			"(",paste(format(quantile.output,digits=digits),collapse=","),")",
			"\n# ",interpretable.label,",missing: ",
			sum(!is.na(biomarker)),"(",format(sum(!is.na(biomarker))/nrow(input.d)*100,digits=digits),"%)",",",
			sum(is.na(biomarker)), "(",format(sum( is.na(biomarker))/nrow(input.d)*100,digits=digits),"%)",
			sep=""),""),
		xlab=xlab.text
	)
}

#################################################################
# function to generate barplot
# NOTE: expect missing to be NA!!! DO NOT filter out missing biomarker data
#       as this function reports missing data count
do.barplot <- function(input.d, data.description, biomarker.var.name, biomarker.name, biomarker.value.names, digits=3) {
	biomarker <- input.d[,biomarker.var.name]
	
	biomarker.value.names.with.count <- apply(
		cbind(biomarker.value.names,as.numeric(table(biomarker))),
		1,
		function(x){
			return(paste(x[1],"\nn=",x[2],sep=""))
		}
	)
	
	barplot(
		table(biomarker), 
		names.arg=biomarker.value.names.with.count,
		ylab="Frequency",
		xlab=biomarker.name,
		main=paste(data.description,"\n# scorable,missing: ",
				sum(!is.na(biomarker)),"(",format(sum(!is.na(biomarker))/nrow(input.d)*100,digits=digits),"%)",",",
				sum(is.na(biomarker)), "(",format(sum( is.na(biomarker))/nrow(input.d)*100,digits=digits),"%)",
				sep="")
	)
}

##################################################################
# function to calculate mean and confidence interval estimated
# by bootstrap
# 
# num.boot = number of bootstrap samples
boot.mean <- function(x,num.boot=1000,random.seed=12,...){
	set.seed(random.seed)
	obs.mean <- mean(x,...)
	ci <- sort(sapply(1:num.boot,function(y){
						boot.x <- sample(x,replace=TRUE)
						return(mean(boot.x,...))
					},USE.NAMES=FALSE))[c(floor(num.boot*0.025),ceiling(num.boot*0.975))]
	return(list("obs.mean"=obs.mean,"ci"=ci,"n"=length(x)))
}


###################################################################
# function to generate stripchart (jitter plot) of 
# a biomarker x some categoriical subtype
# i.e. similar to boxplot
#
# NOTE:
# - expect subtype variable be factor
# - expect biomarker and subtype variable missing to be NA
#
# example of pch ... 20=small dot, 16=bigger dot
#
do.jitter.plot.among.subtypes <- function(input.d,data.description,biomarker.var.name,biomarker.name,subtype.var.name,subtype.name,pch=".",jitter=0.05,digits=3,cex.axis=0.9) {
	
	temp.d <- input.d[
			(!is.na(input.d[,biomarker.var.name])) & 
					(!is.na(input.d[,subtype.var.name])),
	]
	
	biomarker <- temp.d[,biomarker.var.name]
	subtype <- temp.d[,subtype.var.name]
	xbar <- tapply(biomarker,subtype,boot.mean)
	
	test.name <- "Kruskal-Wallis"
	p.value <- kruskal.test(biomarker~subtype)$p.value
	if (length(names(table(subtype)))==2) {
		test.name <- "Wilcoxon Rank Sum"
		p.value <- wilcox.test(biomarker~subtype)$p.value
	}
	par(mar=c(5.1, # bottom margin
			4.1,
			5.1, # top
			2.1))
	stripchart(
			biomarker~subtype,
			method="jitter",
			jitter=jitter,
			pch=pch,
			group.names=paste(
					paste(
							names(xbar),
							rep("\nn=",length(xbar)),
							sep=""
					),
					sapply(xbar,function(x){return(x$n)},USE.NAMES=FALSE),
					sep=""
			),
			ylab=biomarker.name,
			xlab=subtype.name,
			main=paste(
					data.description,
					"\n",test.name," test P=",format(p.value,digits=digits),
					sep=""
			),
			cex.axis=cex.axis,
			vert=TRUE)
	arrows(
			1:length(xbar), 
			sapply(xbar,function(x){return(x$ci[1])},USE.NAMES=FALSE),
			1:length(xbar),
			sapply(xbar,function(x){return(x$ci[2])},USE.NAMES=FALSE),
			angle=90,code=3,length=0.1)
	points(
			sapply(xbar,function(x){return(x$obs.mean)},USE.NAMES=FALSE),
			pch=4, # this is the X symbol
			type="p",
			cex=2)
}


###################################################################
# function to generate stripchart (jitter plot) of 
# a biomarker x some categoriical subtype on top of a boxplot
#
# NOTE:
# - expect subtype variable be factor
# - expect biomarker and subtype variable missing to be NA
#
# example of pch ... 20=small dot, 16=bigger dot
#
do.box.plot.among.subtypes <- function(input.d,data.description,biomarker.var.name,biomarker.name,subtype.var.name,subtype.name,pch=4,jitter=0.1,digits=2,...) {
	temp.d <- input.d[
		(!is.na(input.d[,biomarker.var.name])) & 
		(!is.na(input.d[,subtype.var.name])),
	]
	
	biomarker <- temp.d[,biomarker.var.name]
	if (is.factor(temp.d[,subtype.var.name])) {
		subtype <- droplevels(temp.d[,subtype.var.name])
	} else {
		subtype <- temp.d[,subtype.var.name]
	}
	xbar <- tapply(biomarker,subtype,boot.mean)
	
	test.name <- "Kruskal-Wallis"
	p.value <- kruskal.test(biomarker~subtype)$p.value
	if (length(names(table(subtype)))==2) {
		test.name <- "Wilcoxon Rank Sum"
		p.value <- wilcox.test(biomarker~subtype)$p.value
	}
	
	boxplot(
		biomarker ~ subtype,
		names=paste(
				paste(
						names(xbar),
						rep("\nn=",length(xbar)),
						sep=""
				),
				sapply(xbar,function(x){return(x$n)},USE.NAMES=FALSE),
				sep=""
		),
		ylab=biomarker.name,
		xlab=subtype.name,
		main=paste(
				data.description,
				"\n",test.name," test P=",format(p.value,digits=digits),
				sep=""
		),
		outline=FALSE, # no outliers, they will be drawn by stripchart
		...
	)
	stripchart(jitter(biomarker) ~ subtype,vertical=T,method="jitter",pch=pch,add=T,jitter=jitter)
}