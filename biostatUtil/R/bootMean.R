#' Mean and bootstrap confidence interval 
#' 
#' Finds mean of a vector, with bootstrapped confidence bounds.
#' 
#' Takes a numeric vector and resamples with replacement \code{num.boot} times.
#' If the input vector has any \code{NA} entries, include the argument
#' \code{na.rm = TRUE} from \code{mean}.
#' 
#' @param x a vector (or matrix)
#' @param num.boot number of bootstrap samples. Defaults to 1000.
#' @param conf.level confidence level.
#' @param random.seed random seed for resampling
#' @param ... additional arguments to \code{mean}
#' @return A list with elements
#' \item{obs.mean}{mean}
#' \item{ci}{bootstraped confidence interval}
#' \item{n}{number of bootstrap samples used}
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples 
#' ## Vectors
#' set.seed(344)
#' bootMean(rnorm(100, 5, 3))
#' bootMean(rnorm(100, 5, 3), num.boot = 500)
#' bootMean(rnorm(100, 4, 3), conf.level = 0.90)
#' 
#' ## Missing Values
#' set.seed(344)
#' s <- ifelse(rexp(100, 0.5) < 1, NA, rexp(100, 0.5))
#' bootMean(s)  # doesn't work
#' bootMean(s, na.rm = T)
bootMean <- function(x, num.boot = 1000, conf.level = 0.95, random.seed = 12, ...){
  set.seed(random.seed)
  obs.mean <- mean(x, ...)
  ci <- sort(sapply(1:num.boot, function(y) {
    boot.x <- sample(x, replace = TRUE)
    return(mean(boot.x, ...))},
    USE.NAMES = FALSE))[c(floor(num.boot * (1 - conf.level) / 2),
                          ceiling(num.boot * (1 - (1 - conf.level) / 2)))]
  return(list("obs.mean" = obs.mean, "ci" = ci, "n" = length(x)))
}