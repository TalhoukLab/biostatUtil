kappaBootCI <- function(x, y, seed = 20, num.boot = 1000, conf.level = 0.95) {
  # Function to compute cohen's kappa with binary data with a bootstrap CI
  
  require(boot)
  require(psy)
  set.seed(seed)
  
  ckappa.boot <- function(data, x) {
    ckappa(data[x, ])[[2]]
  }
  
  res <- boot(cbind(x, y), ckappa.boot, num.boot)
  
  # adjusted bootstrap percentile (BCa) confidence interval (better)
  bootCI <- boot.ci(res, type = "bca", conf = conf.level)
  kappa <- c(PointEst = bootCI[[2]], bootCI[[4]][4:5])
  names(kappa)[2:3] <- c(paste0((1 - conf.level) / 2 * 100, "%"),
                         paste0((1 - (1 - conf.level) / 2) * 100, "%"))
  return(kappa)
}