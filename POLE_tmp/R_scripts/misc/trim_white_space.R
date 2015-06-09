###########################################
# function to trim leading and trailing 
# white spaces
#
# 2009-11-19
#
trim.white.spaces <- function(input.text) {
	last.one <- TRUE
	#print(length(input.text))
	if (length(input.text) > 1) {
		remaining.input.text <- input.text[-1]
		input.text <- input.text[1]
		last.one <- FALSE
	}
	
	if (is.na(input.text)) {
		result <- NA
	} else {
		# find position of the first char after leading white spaces
		i <- 1
		max.i <- nchar(input.text)
		c <- substr(input.text, i, i)
	
		while (c == " " & i <= max.i) {
			i <- i+1
			c <- substr(input.text, i, i)
		}
	
		if (i > max.i) {
			return("") # input.text is all whitespace(s)
		} else {
			first.char <- i
		}
	
		# find position of the first char before trailing white spaces
		i <- nchar(input.text)
		min.i <- 1
		c <- substr(input.text, i, i)
	
		while (c == " " & i >= min.i) {
			i <- i-1
			c <- substr(input.text, i, i)
		}
	
		if (i < min.i) {
			return("") # input.text is all whitespace(s)
		} else {
			last.char <- i
		}
	
		result <- substr(input.text, first.char, last.char)
	}
	
	if (last.one) {
		return(result)
	} else {
		return(c(
			result,
			trim.white.spaces(remaining.input.text)
		))
	}
}


