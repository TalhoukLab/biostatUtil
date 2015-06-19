#' Bootstraped mean
#' @param x a vector
#' @param num.boot number of bootstrap samples. Defaults to 1000.
#' @param conf.level confidence level.
#' @param random.seed random seed for resampling
#' @param ... additional arguments to \code{mean}
#' @return A bootstraped mean.
#' @author Aline Talhouk
#' @export
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