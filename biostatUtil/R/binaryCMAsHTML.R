#' Confusion matrix results in HTML
#' 
#' Prints results from \code{\link{binaryCM}} into a nice HTML table format.
#' 
#' @param x vector of reference classes
#' @param y vector of predicted classes
#' @param ref.description description of classes
#' @param digits number of digits to round p-values to
#' @param seed random seed for bootstrap resampling
#' @param num.boot number of bootstrap confidence intervals
#' @param conf.level confidence level. Defaults to 95\%.
#' @param show.ci if \code{TRUE} (default), the confidence intervals are shown.
#' @return A character string that can be parsed as HTML code to display a nice 
#'   confusion matrix summary.
#' @family confusion matrix functions
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples 
#' # 95% CI from 5 bootstraped samples
#' library(htmlTable)
#' set.seed(547)
#' n <- 20
#' x <- rbinom(n, size = 1, prob = 0.6)
#' y <- rbinom(n, size = 1, prob = 0.4)
#' results <- binaryCMAsHTML(x, y, "Test", num.boot = 1000)
#' htmlTable(results)
#' 
#' results.no.ci <- binaryCMAsHTML(x, y, "Test", num.boot = 1000, show.ci =
#' FALSE)
#' htmlTable(results.no.ci)
binaryCMAsHTML <- function(x, y, ref.description, digits = 4, seed = 20,
                           num.boot = 1000, conf.level = 0.95, show.ci = TRUE) {
  td.right <- "text-align: right; white-space: nowrap;"
  td.left <- "text-align: left; white-space: nowrap;"
  ci.label <- ifelse(show.ci, paste0(" (", conf.level * 100, "% CI):"), ":")
  conf.result <- binaryCM(x = as.factor(x), y = as.factor(y), digits = digits,
                          seed = seed, num.boot = num.boot,
                          conf.level = conf.level)
  conf.stats <- subset(conf.result, sapply(conf.result, length) == 3)
  result <- paste0(tags$table(HTML(paste0(
    tags$tr(HTML(paste0(
      tags$td(style = td.right, "Reference:"),
      tags$td(style = td.left, ref.description)
    ))),
    paste(mapply(function(x, n) 
      paste(tags$tr(HTML(paste0(
        tags$td(style = td.right, paste0(n, ci.label)),
        tags$td(style = td.left, ifelse(show.ci, printCI(x), x[1]))
      )))),
      x = conf.stats, n = names(conf.stats), SIMPLIFY = FALSE), collapse = "")
  ))))
  return(result)
}