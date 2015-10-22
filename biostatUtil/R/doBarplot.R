#' Barplot with counts
#' 
#' Generates barplot on variables and shows counts per group.
#' 
#' The number of data points in each group is reported.
#' The number of scorable and missing data are also reported by number
#' and percentage in the barplot title.
#' 
#' @param data data.frame with column names
#' @param var string of variable in \code{data} to graph on
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param title barplot title
#' @param digits number of digits to round to
#' @return Barplot of frequencies for each group
#' @author Samuel Leung, Derek Chiu
#' @note Function expects missing to be \code{NA}. Do not filter out missing data
#' as this function reports missing data frequencies.
#' @export
#' @examples 
#' doBarplot(mtcars, "cyl", "title = Number of cylinders")
#' doBarplot(mtcars, "cyl", "Cylinders", title = "Number of cylinders")
doBarplot <- function(data, var, xlab = var, ylab = "Frequency", title = NULL,
                      digits = 3) {
  dat.var <- data[, var]
  counts <- apply(cbind(names(table(dat.var)), as.numeric(table(dat.var))),
                  1, function(x) paste0(x, collapse = "\nn="))
  n.s <- sum(!is.na(dat.var))
  n.m <- sum(is.na(dat.var))
  barplot(table(dat.var), names.arg = counts, xlab = xlab, ylab = ylab,
          main = paste0(title, "\n# scorable, missing: ", n.s, "(",
                        format(n.s / nrow(data) * 100, digits = digits), "%)",
                        ", ", n.m, "(",
                        format(n.m / nrow(data) * 100, digits = digits), "%)"))
}