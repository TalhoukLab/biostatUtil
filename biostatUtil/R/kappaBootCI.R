#' Boostrapped confidence interval for kappa statistic.
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
#' @return bootstraped confidence interval for Cohen's kappa.
#' @author Aline Talhouk, Derek Chiu
kappaBootCI <- function(x, y, seed = 20, num.boot = 1000, conf.level = 0.95) {
  set.seed(seed)
  
  ckappa.boot <- function(data, x) {
    psy::ckappa(data[x, ])[[2]]
  }
  
  res <- boot(cbind(x, y), ckappa.boot, num.boot)
  bootCI <- boot::boot.ci(res, type = "bca", conf = conf.level)
  kappa <- c(PointEst = bootCI[[2]], bootCI[[4]][4:5])
  names(kappa)[2:3] <- c(paste0((1 - conf.level) / 2 * 100, "%"),
                         paste0((1 - (1 - conf.level) / 2) * 100, "%"))
  return(kappa)
}