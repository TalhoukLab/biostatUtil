#' Do histogram with median
#' @export
##################################################################
# generate histogram, with median, Q1/3
# NOTE: expect missing to be NA!!! DO NOT filter out missing biomarker data
#       as this function reports missing data count
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