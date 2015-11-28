#' Representative simulation of pcNormal
#'
#' @param sim.dat a list object from \code{pcNormal}
#' @param cl cluster memberships
#' @return A list with elements
#' \item{ind}{index of representative simulation}
#' \item{dat}{simulation data representation of all in pcNormal}
#' @author Derek Chiu
#' @export
pcNormalRep <- function(sim.dat, cl) {
  ss <- plyr::ldply(sim.dat, silStats, cl)
  med.ind <- which.min(apply(ss, 1, function(x)
    dist(rbind(c(median(ss$fN), median(ss$aP)), x))))
  med.sim <- sim.dat[med.ind]
  return(list(ind = med.ind, dat = med.sim))
}