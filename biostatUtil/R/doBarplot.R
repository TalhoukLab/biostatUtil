#' Generates barplot
#' 
#' Generates barplot on biomarker variables
#' 
#' @param input.d input data frame
#' @param data.description barplot title description
#' @param biomarker.var.name variable name in \code{input.d} to graph on
#' @param biomarker.name barplot x-axis label
#' @param biomarker.value.names names of biomarker values
#' @param digits number of digits to round to
#' @return Barplot of biomarker frequencies
#' @author Samuel Leung
#' @note Function expects missing to be \code{NA}. Do not filter out missing data
#' as this function reports missing data frequencies.
#' @export
doBarplot <- function(input.d, data.description, biomarker.var.name,
                      biomarker.name, biomarker.value.names, digits = 3) {
  biomarker <- input.d[, biomarker.var.name]
  
  biomarker.value.names.with.count <- apply(
    cbind(biomarker.value.names, as.numeric(table(biomarker))), 1,
    function(x) paste0(x[1], "\nn=", x[2]))
  
  barplot(table(biomarker), 
          names.arg = biomarker.value.names.with.count,
          ylab = "Frequency", xlab = biomarker.name,
          main = paste0(data.description, "\n# scorable,missing: ",
                        sum(!is.na(biomarker)), "(",
                        format(sum(!is.na(biomarker)) / nrow(input.d) * 100,
                               digits = digits), "%)", ",",
                        sum(is.na(biomarker)), "(",
                        format(sum(is.na(biomarker)) / nrow(input.d) * 100,
                               digits = digits), "%)"))
}