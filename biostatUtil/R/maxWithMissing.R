# calculating max with specifying missing values ...
# 
# Author: sleung
###############################################################################

###########################################################################
# return max of x 
# using specified missing value
# NOTE: NA's are ignored
# default.missing.value is the value to return when all items in x are of missing value
#
#' @export
maxWithMissing <- function(x, missing.value = -1, return.missing.value = -1) {
  x.missing <- x %in% missing.value
  
  # all items in x are of missing value
  if (sum(x.missing) == length(x)) {return(return.missing.value)}
  if (sum(is.na(x)) == length(x)) {return(return.missing.value)} # all values are NA's
  
  # return mean of items in x that are not of the missing value
  return (max(as.numeric(x[!x.missing]), na.rm = TRUE))
}


