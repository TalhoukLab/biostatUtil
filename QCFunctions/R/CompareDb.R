#' Compare data frames
#'
#' Compare two data frames. If input is matrix, will try to convert matrix
#' into data fram (via as.data.frame) to compare.
#'
#' @param db.old A data frame.
#' @param db.new A data frame.
#' @param id.var ID variable
#' @param possible.na.strings  A list of possible strings that may represent
#' missing value.  If specified here, CompareDb will consider no change if for
#' example value change from NA to "N/A"
#' @return Prints out a report on the difference between the db's to the console
#' @export
#' @examples
#' a <- cbind("id"=c(1:5),"age"=c(50,60,20,87,41),"grade"=c(1,2,3,1,2))
#' b <- cbind("id"=c(1:5),"age"=c(50,60,20,87,41),"grade"=c("1","2","3","1","2"))
#' c <- cbind("id"=c(1:5),"age"=c(50,60,20,87,40),"grade"=c(1,2,3,2,2))
#' d <- as.data.frame(cbind(c,"nodestat"=c(0,1,1,0,0)))
#' e <- as.data.frame(cbind(a,"LVI"=c(TRUE,FALSE,TRUE,TRUE,FALSE)))
#' f <- a; f[2,"age"] <- NA
#' g <- a; g[2,"age"] <- "N/A"
#' h <- a; h[2,"age"] <- "NA"
#'
#' CompareDb(a,b,"id")
#' CompareDb(a,c,"id")
#' CompareDb(a,d,"id")
#' CompareDb(d,e,"id")
#' CompareDb(a,f,"id")
#' CompareDb(a,g,"id")
#' CompareDb(f,g,"id")
#' CompareDb(f,g,"id",possible.na.strings=c())
#' CompareDb(g,h,"id",possible.na.strings=c())
CompareDb <- function(db.old, db.new, id.var, possible.na.strings = c("NA","na","N/A",NA)) {
  cat("### comparing changes in new dataset ...\n")
  cat("old dataset:\n")
  cat("   n =",nrow(db.old),"\n")
  cat("   number of variables =",ncol(db.old),"\n")
  cat("\n")
  vars.added <- colnames(db.new)[!(colnames(db.new)  %in% colnames(db.old))]
  vars.removed <- colnames(db.old)[!(colnames(db.old)  %in% colnames(db.new))]
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
    cat("rows added:",paste(rows.added,collapse=", "),"\n")
    cat("\n")
  }
  # for each variable, for each cases (not newly added cases), show changes
  existing.vars <- colnames(db.old)[colnames(db.old)%in%colnames(db.new)]
  existing.cases <- db.old[,id.var][db.old[,id.var]%in%db.new[,id.var]]
  existing.case.indexes.in.db.old <- which(db.old[,id.var]%in%existing.cases)
  existing.case.indexes.in.db.new <- match(db.old[existing.case.indexes.in.db.old,id.var],db.new[,id.var])

  changed.vars <- c()
  changed.vars.notices <- c()
  class.changed.notices <- c()
  for (existing.var in existing.vars) {
    # check if class changes for any variables
    old.class <- class(db.old[existing.case.indexes.in.db.old, existing.var])
    new.class <- class(db.new[existing.case.indexes.in.db.new, existing.var])
    if (old.class!=new.class) {
      class.changed.notices <- c(class.changed.notices,
        paste(existing.var,": '",old.class,"' -> '",new.class,"'",sep="")
      )
    }

    old <- db.old[existing.case.indexes.in.db.old, existing.var]; #old <- old[!is.na(old)]
    new <- db.new[existing.case.indexes.in.db.new, existing.var]; #new <- new[!is.na(new)]

	# manually changed some possible string that represents NA and changed them
	# to [blank]
	old[old %in% possible.na.strings] <- ""
	new[new %in% possible.na.strings] <- ""

    num.cases.changed <- sum(old!=new,na.rm=TRUE)
    # also need to tract how many cases changed from NA to non-NA
    num.cases.changed.na <- sum((is.na(old) & !is.na(new)) | (is.na(new) & !is.na(old)))
    num.cases.changed <- num.cases.changed + num.cases.changed.na
    if ((num.cases.changed) != 0) {
      # need to check if variable is numeric, if so, it could be difference in
      # trailing zero's that caused the difference
      if (isParsableToNumeric(old) & isParsableToNumeric(new)) {
		  old <- as.character(sapply(as.numeric(old),function(x){
		    return(ifelse(is.na(x),"",x))
          }))
		  new <- as.character(sapply(as.numeric(new),function(x){
            return(ifelse(is.na(x),"",x))
          }))
		  num.cases.changed <- num.cases.changed.na + sum(old!=new,na.rm=TRUE)
	  }
	  if (num.cases.changed != 0) {
        changed.vars <- c(changed.vars, existing.var)
        changed.vars.notices <- c(changed.vars.notices,
          paste(existing.var,": ",num.cases.changed," case(s) CHANGED!!!",sep="")
        )
      }
    }
  }

  if (length(class.changed.notices)>0) {
    cat("# showing variables with changed class ...\n")
    for (class.changed.notice in class.changed.notices) {
      cat("   ",class.changed.notice,"\n")
    }
    cat("# end of variables with changed class\n\n")
  }

  if (length(changed.vars)>0) {
    cat("# showing summary of variables with changed values ...\n")
    for (changed.var.notice in changed.vars.notices) {
      cat("   ",changed.var.notice,"\n")
    }
    cat("# end of summary of variables with changed values\n\n")

    # showing details of changes ...
    cat("# showing details of changed variables values ...\n")
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
        need.show.change <- FALSE
        if (is.na(old[i]) | is.na(new[i])) {
          need.show.change <- TRUE
        } else if (old[i] != new[i]) {
          if (suppressWarnings(!is.na(as.numeric(old[i]))) & suppressWarnings(!is.na(as.numeric(new[i])))) {
            # both are numeric ... change them to numeric first
            old[i] <- as.numeric(old[i])
            new[i] <- as.numeric(new[i])
          }
          if (old[i] != new[i]) {
            need.show.change <- TRUE
          }
        }
        if (need.show.change) {
          old.value.to.show <- ifelse(!is.na(old[i]),paste("'",old[i],"'",sep=""),as.character(old[i]))
          new.value.to.show <- ifelse(!is.na(new[i]),paste("'",new[i],"'",sep=""),as.character(new[i]))
          changes <- c(changes, paste(old.value.to.show," -> ",new.value.to.show,sep=""))
        }
      }
      changes.table <- table(changes)
      for (change.desc in apply(cbind(as.numeric(changes.table),names(changes.table)),1,function(x){return(paste(x[1]," cases: ",x[2],sep=""))})) {
        cat("   ",change.desc,"\n")
      }
    }
    cat("# end of details of changed variables.\n\n")
  }
  cat("### end of comparing changes in new dataset.\n")
}


