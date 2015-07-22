#' Generates barplot
#' @export
##################################################################
# function to generate barplot
# NOTE: expect missing to be NA!!! DO NOT filter out missing biomarker data
#       as this function reports missing data count
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
                        format(sum(!is.na(biomarker)) /nrow(input.d) * 100,
                               digits = digits), "%)", ",",
                        sum(is.na(biomarker)), "(",
                        format(sum(is.na(biomarker)) / nrow(input.d) * 100,
                               digits = digits), "%)"))
}