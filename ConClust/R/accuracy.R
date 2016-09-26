#' Classification Accuracy
#'
#' Given a sorted table, the accuracy is calculated based on proportion
#' correctly classified.
#'
#' The table must have rows and columns corresponding to the same classes. Then
#' the classification accuracy is computed as the sum of the diagonal entries
#' divided by the sum of all entries in the confusion matrix comparing the
#' predicted and reference classes.
#'
#' @param tbl table with predicted and reference classes correctly matched
#' @return classification accuracy, given as a proportion
#' @author Derek Chiu
#' @export
#' @examples
#' set.seed(1)
#' x <- matrix(rbinom(16, 20, 0.4), nrow = 4)
#' accuracy(x)
accuracy <- function(tbl) {
  return(sum(diag(tbl)) / sum(tbl))
}
