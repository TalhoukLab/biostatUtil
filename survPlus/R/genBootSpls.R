#' Standard deviation of log hazard ratio
#'
#' Compute the standard deviation of a log hazard ratio when only given its
#' repoted confidence limits.
#'
#' Extract standard deviation from a confidence limit and obtain a
#' corresponding p-value, which can be compared to the reported p-value.
#' The null p-values are calculated from the Chi-squared 1 distribution.


#' Generating Bootstrap Samples of a dataset
#'
#' Generate bootstrap samples by sampling with replacement. The bootstrap samples and the
#' unselected samples are saved seperately.
#' @param dat, the data matrix that needs to be bootstrapped
#' @param B, the number of bootstrap samples desired (default to 1000)
#' @return a litst with with boot.tr being the bootstrap samples and boot.te being the unseleceted samples
#' @author Aline Talhouk, Derek Chiu
#' @export

genBootSpls <- function(dat, B=1000){
  N <- nrow(dat)
  boot.dat <- plyr::rlply(B, {
    tr.ind <- sample(1:N, replace = TRUE)
    te.ind <- which(!(1:N %in% tr.ind))
    dat.train <- dat[tr.ind, ]
    dat.test <- dat[te.ind, ]
    list(tr = dat.train, te = dat.test)
  })

  boot.tr <- lapply(boot.dat, function(x) x$tr)
  boot.te <- lapply(boot.dat, function(x) x$te)
  return(list(boot.tr=boot.tr,boot.te=boot.te))
}
