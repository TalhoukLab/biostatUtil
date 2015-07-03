
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
