#' Do histogram with median
#' 
#' Plot a histogram with the median, first quartile, and third quartile reported.
#' 
#' Expects missing to be \code{NA}. Do not filter out missing biomarker data
#' as this function reports missing data counts.
#' 
#' @param input.d input \code{data.frame}
#' @param data.description title description
#' @param biomarker.var.name variable name of biomarker to plot
#' @param biomarker.name x-axis label for biomarker name
#' @param show.title logical. If \code{TRUE} (default), the title is shown
#' @param br breaks in histogram
#' @param digits number of digits to round for median, Q1, Q3
#' @param interpretable.label label for non-missing cases
#' @return A histogram with some annotated values.
#' @author Samuel Leung
#' @export
doHist <- function(input.d, data.description, biomarker.var.name,
                   biomarker.name, show.title = TRUE, br = 100, digits = 3,
                   interpretable.label = "scorable") {
  xlab.text <- biomarker.name
  biomarker <- input.d[, biomarker.var.name]
  quantile.output <- quantile(biomarker, na.rm = TRUE)
  data.description <- ifelse(is.na(data.description), "",
                             paste0(data.description, "\n"))
  hist(biomarker, br = br,
       main = ifelse(show.title, paste0(
         data.description, "mean (min,Q1,median,Q3,max): ",
         format(mean(biomarker, na.rm = TRUE), digits = digits), " ", "(",
         paste(format(quantile.output, digits = digits),
               collapse = ","),
         ")", "\n# ", interpretable.label, ",missing: ",
         sum(!is.na(biomarker)), "(",
         format(sum(!is.na(biomarker)) / nrow(input.d) * 100, digits = digits),
         "%)", ",",
         sum(is.na(biomarker)), "(",
         format(sum( is.na(biomarker)) / nrow(input.d) * 100, digits = digits),
         "%)"), ""),
       xlab = xlab.text)
}