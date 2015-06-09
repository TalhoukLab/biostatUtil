# misc helper functions
# 
# Author: samuelc
###############################################################################

# TABLE OF CONTENTS
# 0. constants
# 1. numeric helpr functions
# 2. string related functions
# 3. html related functions

#############################################################
### 0. constants                                          ###
COL.TH.STYLE <- "border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;"
ROW.TH.STYLE <- "text-align: center; padding-right:10px; padding-right:10px;"
TABLE.CAPTION.STYLE <- "display: table-caption; text-align: left;"
### end of constants                                      ###
#############################################################

#############################################################
### 1. numeric helper functions                           ###

################################################
# median center rows (gene) of the data matrix
# so that each gene have median approx. 0 
#
# x: matrix where rownames = genes names, column names = samples names
# 
medianCtr<-function(x){
	annAll <- dimnames(x)
	medians <- apply(x,1,median,na.rm=T) # calculate median for each row
	
	# center each row of matrix so that all rows (genes) have a median of approx. 0
	x <- t(scale(t(x),center=medians,scale=F)) # scale is generic function whose default method 
	# centers and/or scales the COLUMNS of a numeric matrix. 
	dimnames(x) <- annAll # reset the row/column names back to original ones
	return(x)
}

###################################################################################
# standardize a matrix by centering and scaling the columns (samples) of the matrix
# so that all columns (samples) have approx the same mean and range
#
# x: matrix
#
standardize<-function(x){
	annAll<-dimnames(x)
	x<-scale(x) # scale is generic function whose DEFAULT method 
	# CENTERS and SCALES the columns of a numeric matrix.
	# - centering is done by subtracting the column means 
	#   (omitting NAs) of x from their corresponding columns
	# - scaling is done by dividing the (centered) 
	#   columns of x by their standard deviations
	dimnames(x)<-annAll # reset row/column names back to original
	return(x)
}

########################################
# calculate geo mean
#
geo.mean <- function(x) {
	x <- x[!is.na(x)]
	result <- NA
	if (length(x) > 0) {
		result <- prod(x)^(1/length(x))
	}
	return(result)
}

#########################################
# find standard error of the mean (SEM) 
# 
# ref: http://en.wikipedia.org/wiki/Standard_error
sem <- function(x, missing.value=NA, return.missing.value=NA) { 
	# remove missing values ...
	x <- x[!is.na(x)]
	if (!is.na(missing.value)) {
		x <- x[!x%in%missing.value]
	}
	return(ifelse(
		length(x)==0,
		return.missing.value,
		sqrt(var(x)/length(x))
	))
}

### end of numeric helper function                        ###
#############################################################

#############################################################
### 2. string helper functions                            ###

#########################################
# capitalize first letter 
#
# e.g. "clear cell" -> "Clear Cell"
#
# ref: http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string
simpleCap <- function(x, first.word.only=FALSE) {
	if (!first.word.only) {
		s <- strsplit(x, " ")[[1]]
	} else {
		s <- x
	}
	return(paste(toupper(substring(s, 1,1)), substring(s, 2),sep="", collapse=" "))
}

###########################################
# check if the first letter to the input string is
# uppercase
#
# note: empty string, returns TRUE
isFirstLetterUpperCase <- function(x) {
	arr <- strsplit(x,"")[[1]]
	if (length(arr)==0) {
		return(TRUE) # empty string - returns TRUE
	} else {
		return(arr[1]==toupper(arr[1]))
	}
}

##########################################
# 
# check to see if the string a ends with the string b
#
endsWith <- function(a,b) {
	if (length(grep(b,a))==0) {
		return(FALSE) # not even substring ... must be false
	} else {
		return(substr(a,nchar(a)-nchar(b)+1,nchar(a))==b)
	}
}
#
# check to see if the string a starts with the string b
startsWith <- function(a,b) {
	if (length(grep(b,a))==0) {
		return(FALSE) # not even substring ... must be false
	} else {
		return(substr(a,1,nchar(b))==b)
	}
}
#
# find occurrence(s) substring index of b in a
# return NA if not found
indexOf <- function(a,b,ignore.case=FALSE) {
	if (ignore.case) {
		a <- toupper(a)
		b <- toupper(b)
	}
	b.len <- nchar(b)
	a.len <- nchar(a)
	if (b.len>a.len) {
		return(NA) # b is longer than a, therefore, not possible for b to be found in a
	}
	a.arr <- strsplit(a,"")[[1]]
	indexes <- c()
	i <- 1
	while (i <= (a.len-b.len+1)) {
		if  (paste(a.arr[i:(i+b.len-1)],collapse="")==b) {
			indexes <- c(indexes,i)
			i <- i+b.len-1
		}
		i <- i+1
	}
	if (length(indexes)>0) {
		return(indexes)
	} else {
		return(NA)
	}
}
#
# escape a string for grep
escapeForGrep <- function(x){
	sub("\\[","\\\\[",
		sub("\\]","\\\\]",
			sub("\\(","\\\\(",
				sub("\\)","\\\\)",x)
	)))
}
#
#
### end of string helper functions                        ###
#############################################################

#####################################
### 3. html related functions     ###
#

###############################
# add table number to caption
#
add.table.number <- function(caption, default.table_counter_str="Table %s: ") {
	# generate table number ...
	if (!is.na(caption)) {
		tc <- getOption("table_counter")
		if (is.numeric(tc)) {
			tc <- tc + 1		
		} else {
			tc <- 1
		}
		options(table_counter=tc)
		caption_template <- getOption("table_counter_str", default.table_counter_str)
		caption <- paste(sprintf(caption_template,as.character(tc)), caption, sep="")
	} 
	return(caption)
}


##########################################
# 
# generate html code for the input table
# assume table has > 1 rows
#
table.as.html <- function(
	t, 
	row.names=NULL,
	column.names=NULL,
	caption=NA,
	html.table.border=0,
	banded.rows=FALSE,
	css.class.name.odd="odd",
	css.class.name.even="even"
) {
	th.style <- "border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;"
	result <- paste("<table border=",html.table.border,">",ifelse(is.na(caption),"",paste("<caption style='",TABLE.CAPTION.STYLE,"'>",add.table.number(caption),"</caption>",sep="")),sep="")
	# print header
	if (!is.null(row.names)) {
		rownames(t) <- row.names
	} else {
		row.names <- rownames(t)
	}
	if (!is.null(column.names)) {
		colnames(t) <- column.names
	} else {
		column.names <- colnames(t)
	}
	result <- paste(result,"<tr><th style='",th.style,"'></th><th style='",th.style,"'>",paste(column.names,collapse=paste("</th><th style='",th.style,"'>",sep="")),"</th></tr>",sep="")
	for (i in 1:nrow(t)) {
		tr.class <- ifelse(banded.rows,paste(" class='",ifelse(i%%2==0,css.class.name.even,css.class.name.odd),"'",sep=""),"")
		result <- paste(result,"<tr",tr.class,"><th>",row.names[i],"</th><td>",paste(t[i,],collapse="</td><td>"),"</td></tr>",sep="")
	}
	result <- paste(result,"</table>",sep="")
	return(result)
}


### end of html related functions ###
#####################################