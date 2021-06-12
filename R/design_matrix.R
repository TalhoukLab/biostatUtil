#' Design Matrix with interactions
#'
#' A design matrix with desired order of interactions.
#'
#' @param data numeric data
#' @param order complexity of interactions. Second order interactions would
#'   comprise of all pairs of variables, third order interactions include all
#'   triplets of variables, and so on. Defaults to 2.
#' @return A design matrix with desired order of interactions for all variables
#' @author Derek Chiu
#' @export
#' @examples
#' set.seed(2021)
#' df <- data.frame(matrix(rnorm(40), ncol = 4))
#' design_matrix(df)
design_matrix <- function(data, order = 2) {
  nms <- names(data)
  x <- c(paste0("(", nms[1]),
         nms[-c(1, length(nms))],
         paste0(nms[length(nms)], ")^", order))
  f <- arsenal::formulize(x = x, collapse = "+")
  stats::model.matrix(f, data)
}
