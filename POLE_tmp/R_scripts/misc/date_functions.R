###############################################################
### format dates 
#
# make date format consistent to MM/DD/YYYY
# - it seems date format are not consistent for 10-005,10-006,11-010 ... intra and inter cohort both inconsistent
#
# ASSUME format is either
# - MM/DD/YYYY
# - DD/MM/YYYY
# - YYYY/MM/DD (assume YYYY/DD/MM is never used)
# - YYYYMMDD
#
# WARNING this function only works if preferred.format is either MM/DD/YYYY OR DD/MM/YYYY!!!!
clean.date.string.and.format <- function(x,original.format,preferred.format,existing.missing.codes=c(""),return.missing.code=NA){
	if (is.na(x)) {return(return.missing.code)}
	x <- trim.white.spaces(x)
	if (x %in% existing.missing.codes){return(x)} # preserve error coding
	# need to break the date string into components 
	date.comp <- strsplit(x,"/|-")[[1]]
	temp <- suppressWarnings(as.numeric(date.comp[1]))
	if (is.na(temp)) {
		#cat("WARNING (clean.date.string.and.format): trying to format a non-standard date string: ",x,"\n")
		return(return.missing.code)
	}
	if (temp > 1000000) {
		# this would a format of yyyymmdd or ddmmyyyy or mmddyyyy
		temp <- paste(ifelse(temp < 10000000,"0",""),temp,sep="")# 7 or 8-digits; turn temp back to string
		if (as.numeric(substr(temp,5,6))<13) { # assume we will NOT be looking at any dates from 1200's!!!!
			# must be YYYYMMDD
			return(format.date.string.from.day.month.year(substr(temp,7,8),substr(temp,5,6),substr(temp,1,4),date.format=preferred.format))
		} else if (as.numeric(substr(temp,1,2))>12) {
			# must be DDMMYYYY
			return(format.date.string.from.day.month.year(substr(temp,1,2),substr(temp,3,4),substr(temp,5,8),date.format=preferred.format))
		} else if (as.numeric(substr(temp,3,4))>12) {
			# must be MMDDYYYY
			return(format.date.string.from.day.month.year(substr(temp,3,4),substr(temp,1,2),substr(temp,5,8),date.format=preferred.format))
		} else if (original.format==DDMMYYYY) {
			return(format.date.string.from.day.month.year(substr(temp,1,2),substr(temp,3,4),substr(temp,5,8),date.format=preferred.format))
		} else { 
			# assume must be MMDDYYYY
			return(format.date.string.from.day.month.year(substr(temp,3,4),substr(temp,1,2),substr(temp,5,8),date.format=preferred.format))
		}
	} else if (temp > 31) {
		# must be YYYY/MM/DD because no day can be > 31
		return(format.date.string.from.day.month.year(date.comp[3],date.comp[2],date.comp[1],date.format=preferred.format))
	} else if (temp > 12) {
		# must be DD/MM/YYYY
		return(format.date.string.from.day.month.year(date.comp[1],date.comp[2],date.comp[3],date.format=preferred.format))
	} else { # first component is <= 12 ... however, we are not sure if it refers to a day or month
		temp <- as.numeric(date.comp[2]) # second component can either be a day or month
		if (temp > 12) {
			# must be MM/DD/YYYY
			return(format.date.string.from.day.month.year(date.comp[2],date.comp[1],date.comp[3],date.format=preferred.format))
		} else { # BOTH first and second component is <=12
			# assume its in the original format
			if (original.format==MM.DD.YYYY) {
				return(format.date.string.from.day.month.year(date.comp[2],date.comp[1],date.comp[3],date.format=preferred.format))
			} else if (original.format==DD.MM.YYYY) {
				return(format.date.string.from.day.month.year(date.comp[1],date.comp[2],date.comp[3],date.format=preferred.format))
			} else {
				cat("ERROR (clean.date.string.and.format): unknown original.format specified: ",original.format,"\n")
				return(NA)
			}
		}
	}
}

################
# calculate date difference between two date string d1 and d2 i.e. d1-d2
#
# units = c("auto", "secs", "mins", "hours", "days", "weeks","months","years")
#
diff.date.string <- function(d1,d2,date.format=MM.DD.YYYY,units='days',existing.missing.codes=NA,return.missing.code=NA) {
	if(is.na(d1)) {return(NA)}
	if(is.na(d2)) {return(NA)}
	if (length(unique(existing.missing.codes[!is.na(existing.missing.codes)]))>0) {
		if (d1 %in% existing.missing.codes) {return(return.missing.code)}
		if (d2 %in% existing.missing.codes) {return(return.missing.code)}
	}
	result <- as.numeric(
		difftime(
			strptime(clean.date.string.and.format(d1,date.format,date.format,existing.missing.codes=existing.missing.codes,return.missing.code=return.missing.code),format=date.format),
			strptime(clean.date.string.and.format(d2,date.format,date.format,existing.missing.codes=existing.missing.codes,return.missing.code=return.missing.code),format=date.format),
			units=ifelse(units %in% c("months","years"),"days",units)))
	if (units=="months") {
		return(result/NUM.DAYS.IN.MONTH)
	} else if (units=="years") {
		return(result/NUM.DAYS.IN.YEAR)
	} else {
		return(result)
	}
}

################
# compare two date string d1 and d2 
# return  1 if d1 > d2
#        -1 if d1 < d2
#         0 if d1 == d2
#
# units = c("auto", "secs", "mins", "hours", "days", "weeks","months","years")
#
compare.date.string <- function(d1,d2,date.format=MM.DD.YYYY,units='days',existing.missing.codes=NA,return.missing.code=NA) {
	difference <- diff.date.string(d1,d2,date.format=date.format,units=units,existing.missing.codes=existing.missing.codes,return.missing.code=return.missing.code)
	if (is.na(return.missing.code)) {
		if (is.na(difference)) {return(NA)}
	} else {
		if (difference==return.missing.code) {return(return.missing.code)}
	}
	if (difference > 0) {
		return(1)
	} else if (difference < 0) {
		return(-1)
	} else {
		return(0)
	}
}

################
# add to a date string and return a date string
# assume input date is in the specified format
# output date string will be in the specified format as well
# - expects missing value to NA
#
add.to.date.string <- function(org.date, delta, date.format=MM.DD.YYYY, units='days',existing.missing.codes=NA,return.missing.code=NA) {
	if (is.na(org.date)) {return(NA)}
	if (is.na(delta))    {return(NA)}
	if (length(unique(existing.missing.codes[!is.na(existing.missing.codes)]))>0) {
		if (org.date %in% existing.missing.codes) {return(return.missing.code)}
		if (delta    %in% existing.missing.codes) {return(return.missing.code)}
	}
	delta <- as.numeric(delta)
	delta.in.days <- delta
	if (units == "weeks") {
		delta.in.days <- delta * 7
	} else if (units == "months") {
		delta.in.days <- delta * NUM.DAYS.IN.MONTH
	} else if (units == "years") {
		delta.in.days <- delta * NUM.DAYS.IN.YEAR
	}
	return(
		format(
			as.Date(
				(
					as.Date(
						clean.date.string.and.format(org.date,date.format,date.format,existing.missing.codes=existing.missing.codes,return.missing.code=return.missing.code),
						format=date.format,origin=DATE.ORIGIN
					) + 
					delta.in.days
				), 
				origin=DATE.ORIGIN
			),
			format=date.format
		)
	)
}

################
# find the maximum (latest) of an array of date string
# - expects missing value to NA
#
max.date.string <- function(t.arr, date.format=MM.DD.YYYY,existing.missing.codes=NA,return.missing.code=NA) {
	t.arr <- t.arr[!is.na(t.arr)]
	if (length(unique(existing.missing.codes[!is.na(existing.missing.codes)]))>0) {
		t.arr <- t.arr[!(t.arr %in% existing.missing.codes)]
	}
	if (length(t.arr)==0) {
		return(return.missing.code)
	}
	return(format(as.Date(max(sapply(t.arr,function(x){
		return(
			as.Date(
				clean.date.string.and.format(x,date.format,date.format,existing.missing.codes=existing.missing.codes,return.missing.code=return.missing.code),
				format=date.format,origin=DATE.ORIGIN))
	},USE.NAMES=FALSE)),origin=DATE.ORIGIN),format=date.format))
}

################
# find the minimum (earliest) of an array of date string
# - expects missing value to NA
#
min.date.string <- function(t.arr, date.format=MM.DD.YYYY,existing.missing.codes=NA,return.missing.code=NA) {
	t.arr <- t.arr[!is.na(t.arr)]
	if (length(unique(existing.missing.codes[!is.na(existing.missing.codes)]))>0) {
		t.arr <- t.arr[!(t.arr %in% existing.missing.codes)]
	}
	if (length(t.arr)==0) {
		return(return.missing.code)
	}
	return(format(as.Date(min(sapply(t.arr,function(x){
		return(
			as.Date(
				clean.date.string.and.format(x,date.format,date.format,existing.missing.codes=existing.missing.codes,return.missing.code=return.missing.code),
				format=date.format,origin=DATE.ORIGIN
			)
		)
	},USE.NAMES=FALSE)),origin=DATE.ORIGIN),format=date.format))
}
