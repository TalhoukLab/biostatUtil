#' Do histogram with median
#' 
#' Plot a histogram with the median, first quartile, and third quartile reported.
#' 
#' Expects missing to be \code{NA}. Do not filter out missing data
#' as this function reports missing data counts.
#' 
#' @param data data.frame with column names
#' @param var string of variable in \code{data} to graph on
#' @param xlab x-axis label
#' @param title histogram title
#' @param show.title logical. If \code{TRUE} (default), the title is shown
#' @param br breaks in histogram. By default, the number of bins is taking as
#' the ceiling of the square root of the number of rows in \code{data}
#' @param digits number of digits to round for median, Q1, Q3
#' @param score.lab label for non-missing cases
#' @param ... additional arguments to \code{hist}
#' @return A histogram with some annotated values.
#' @author Samuel Leung, Derek Chiu
#' @importFrom graphics hist lines mtext
#' @importFrom stats dnorm
#' @export
#' @examples 
#' doHist(mtcars, "mpg")
#' doHist(mtcars, "mpg", "MPG")
#' doHist(mtcars, "mpg", title = "Distribution of MPG")
doHist <- function(data, var, xlab = var, title = NULL, show.title = TRUE,
                   br = ceiling(sqrt(nrow(data))), digits = 3,
                   score.lab = "scorable", ...) {
  dat.var <- data[, var]
  qs <- quantile(dat.var, na.rm = TRUE)
  n.s <- sum(!is.na(dat.var))
  n.m <- sum(is.na(dat.var))
  if (show.title) {
    title <- ifelse(!is.null(title), paste0(title, "\n"), "")
    main <- paste0(
      title, "Mean (Min, Q1, Median, Q3, Max): ",
      format(mean(dat.var, na.rm = TRUE), digits = digits), " ", "(",
      paste(format(qs, digits = digits), collapse = ", "), ")",
      "\n# ", score.lab, ", missing: ", n.s, "(",
      format(n.s / nrow(data) * 100, digits = digits), "%)", ", ", n.m, "(",
      format(n.m / nrow(data) * 100, digits = digits), "%)")
  } else {
    main <- ""
  }
  hist(dat.var, br = br, xlab = xlab, main = main, ...)
}