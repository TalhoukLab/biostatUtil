#' Bootstrapped confidence interval for kappa statistic.
#' 
#' Function to compute cohen's kappa with binary data with a bootstrap confidence interval
#' 
#' Cohen's kappa measures the amount of agreement between 2 raters of a binary variable.
#' The bootstrap confidence interval is adjusted (BCa).
#' 
#' @param x vector of binary scores from first rater
#' @param y vector of binary scores from second rater
#' @param seed random seed for bootstrapping
#' @param num.boot number of times to bootstrap. Defaults to 1000.
#' @param conf.level confidence level. Defaults to 95\%.
#' @param method statistic to calculate bootstrap estimate
#' @param type method setting for \code{method = "krippendorff"}
#' @return bootstraped confidence interval for Cohen's kappa.
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples 
#' a <- rbinom(n = 100, size = 1, prob = 0.3)
#' b <- rbinom(n = 100, size = 1, prob = 0.7)
#' kappaBootCI(a, b)
#' 
#' ## Use a different seed
#' kappaBootCI(a, b, 5)
kappaBootCI <- function(x, y, seed = 20, num.boot = 1000, conf.level = 0.95,
                        method = "cohen", type = "nominal") {
  set.seed(seed)
  
  ckappa.boot <- function(data, x) {
    psy::ckappa(data[x, ])[[2]]
  }
  wkappa.boot <- function(data, x,weight = "squared") {
    irr::kappa2(data[x, ], weight)$value
  }
  fkappa.boot <- function(data, x) {
    irr::kappam.fleiss(data[x, ])$value
  }
  kripalpha.boot <- function(data, x, method = type) {
    irr::kripp.alpha(data[x, ], method)$value
  }
  if (method == "cohen") { 
    fun <- ckappa.boot
  } else if (method == "weighted") {
    fun <- wkappa.boot
  } else if (method == "fleiss") {
    fun <- fkappa.boot
  } else if (method == "krippendorff") {
    fun <- kripalpha.boot
  }
  res <- boot::boot(cbind(x, y), fun, num.boot)
  bootCI <- boot::boot.ci(res, type = "bca", conf = conf.level)
  kappa <- c(PointEst = bootCI[[2]], bootCI[[4]][4:5])
  names(kappa)[2:3] <- c(paste0((1 - conf.level) / 2 * 100, "%"),
                         paste0((1 - (1 - conf.level) / 2) * 100, "%"))
  return(kappa)
}
