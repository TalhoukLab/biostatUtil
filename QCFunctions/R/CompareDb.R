require(stringr)
#' Compare two data frames.  If input is matrix, will try to convert matrix 
#' into data fram (via as.data.frame) to compare.
#' 
#' @param db.old A data frame.
#' @param db.new A data frame.
#' @return Prints out a report on the difference between the db's to the console
#' @examples
#' a <- cbind("id"=c(1:5),"age"=c(50,60,20,87,41),"grade"=c(1,2,3,1,2))
#' b <- cbind("id"=c(1:5),"age"=c(50,60,20,87,41),"grade"=c(1,2,3,1,2))
#' c <- cbind("id"=c(1:5),"age"=c(50,60,20,87,40),"grade"=c(1,2,3,2,2))
#' d <- as.data.frame(cbind(c,"nodestat"=c(0,1,1,0,0)))
#' CompareDb(a,b,"id")
#' CompareDb(a,c,"id")
#' CompareDb(a,d,"id")

CompareDb <- function(db.old, db.new, id.var) {
  if (!is.data.frame(db.old)) {
    db.old <- as.data.frame(db.old)
  }
  if (!is.data.frame(db.new)) {
    db.new <- as.data.frame(db.new)
  }
  cat("### comparing changes in new dataset ...\n")
  cat("old dataset:\n")
  cat("   n =",nrow(db.old),"\n")
  cat("   number of variables =",ncol(db.old),"\n")
  cat("\n")
  vars.added <- names(db.new)[!(names(db.new)  %in% names(db.old))]
  vars.removed <- names(db.old)[!(names(db.old)  %in% names(db.new))]
  rows.added <- db.new[!(db.new[,id.var]%in%db.old[,id.var]),id.var]
  rows.removed <- db.old[!(db.old[,id.var]%in%db.new[,id.var]),id.var]
  cat("new dataset:\n")
  cat("   n =",                  nrow(db.new),"; ",length(rows.removed),"rows removed; ",length(rows.added),"added\n")
  cat("   number of variables =",ncol(db.new),"; ",length(vars.removed),"removed; ",     length(vars.added),"added\n")
  cat("\n")
  if (length(vars.removed)>0) {
    cat("variables removed:",paste(vars.removed,collapse=", "),"\n")
    cat("\n")
  }
  if (length(vars.added)>0) {
    cat("variables added:",paste(vars.added,collapse=", "),"\n")
    cat("\n")
  }
  if (length(rows.removed)>0) {
    cat("rows removed:",paste(rows.removed,collapse=", "),"\n")
    cat("\n")
  }
  if (length(rows.added)>0) {
    cat("variables added:",paste(rows.added,collapse=", "),"\n")
    cat("\n")
  }
  # for each variable, for each cases (not newly added cases), show changes
  existing.vars <- names(db.old)[names(db.old)%in%names(db.new)]
  existing.cases <- db.old[,id.var][db.old[,id.var]%in%db.new[,id.var]]
  existing.case.indexes.in.db.old <- which(db.old[,id.var]%in%existing.cases)
  existing.case.indexes.in.db.new <- match(db.old[existing.case.indexes.in.db.old,id.var],db.new[,id.var])
  # possible strings that may represent NA ... want to change all of them to 
  # blank for comparison ... i.e. consider no change if value change from
  # NA to "N/A"
  possible.na.strings <- c("NA","na","N/A",NA)
  changed.vars <- c()
  for (existing.var in existing.vars) {
    old <- db.old[existing.case.indexes.in.db.old, existing.var]; #old <- old[!is.na(old)]
    new <- db.new[existing.case.indexes.in.db.new, existing.var]; #new <- new[!is.na(new)]
	
	# manually changed some possible string that represents NA and changed them 
	# to [blank]
	old[old %in% possible.na.strings] <- ""
	new[new %in% possible.na.strings] <- ""
	
    num.cases.changed <- sum(old!=new,na.rm=TRUE)
    if (num.cases.changed != 0) {
      # need to check if variable is numeric, if so, it could be difference in 
      # trailing zero's that caused the difference 
      if (IsParsableToNumeric(old) & IsParsableToNumeric(new)) {
		  old <- as.character(sapply(as.numeric(old),function(x){
		    return(ifelse(is.na(x),"",x))
          }))
		  new <- as.character(sapply(as.numeric(new),function(x){
            return(ifelse(is.na(x),"",x))
          }))
		  num.cases.changed <- sum(old!=new,na.rm=TRUE)
	  }
	  if (num.cases.changed != 0) {
        changed.vars <- c(changed.vars, existing.var)
        cat(existing.var,": ",num.cases.changed," case(s) CHANGED!!!\n")
      }
    }
  }
  if (length(changed.vars)>0) {
    # showing details of changes ...
    cat("# showing details of changed variables ...\n")
    for (changed.var in changed.vars) {
      old <- db.old[existing.case.indexes.in.db.old, changed.var] 
      new <- db.new[existing.case.indexes.in.db.new, changed.var]
	  
	  # manually changed some possible string that represents NA and changed them 
	  # to [blank]
	  old[old %in% possible.na.strings] <- ""
	  new[new %in% possible.na.strings] <- ""

      cat(changed.var,":\n")
      changes <- c()
      for (i in 1:length(old)) {
        if (old[i] != new[i]) {
          if (suppressWarnings(!is.na(as.numeric(old[i]))) & suppressWarnings(!is.na(as.numeric(new[i])))) {
            # both are numeric ... change them to numeric first
            old[i] <- as.numeric(old[i])
            new[i] <- as.numeric(new[i])
          }
          if (old[i] != new[i]) {
            changes <- c(changes, paste("'",as.character(old[i]),"' -> '",as.character(new[i]),"'",sep=""))
          }
        }
      }
      changes.table <- table(changes)
      for (change.desc in apply(cbind(as.numeric(changes.table),names(changes.table)),1,function(x){return(paste(x[1]," cases: ",x[2],sep=""))})) {
        cat("   ",change.desc,"\n")
      }
    }
    cat("# end of details of changed variables.\n")
  }
  cat("### end of comparing changes in new dataset.\n")
}

#############################################
# return true if the x is parsable to numeric i.e. it is either already in 
# numeric format or the string values represent number
# - blanks are ignore
#
IsParsableToNumeric <- function(x) {
  if (is.numeric(x)) {
    return(TRUE)
  }
  # 1. remove all blanks
  x <- sapply(x,str_trim)
  x <- x[(!is.na(x)) & (x!="")]
  return(sum(suppressWarnings(!is.na(as.numeric(x))))==length(x))
}
