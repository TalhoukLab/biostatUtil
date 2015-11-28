#' Range of cluster strengths from pcNormal simulations
#'
#' @param sim.dat a list object from \code{pcNormal}
#' @param cl cluster memberships
#' @param int every \code{int} data sets from median ranked \code{sim.dat}
#' are taken. Takes every 5th dataset by default.
#' @return A list with elements
#' \item{ranks}{ranks of each extracted dataset}
#' \item{ind}{indices of range of median ranked datasets}
#' \item{dat}{list of simulated data cover range of median ranks}
#' @author Derek Chiu
#' @export
pcNormalRange <- function(sim.dat, cl, int = 5) {
  ss <- plyr::ldply(sim.dat, silStats, cl)
  rks <- seq(1 + int, length(sim.dat), int)
  med.order <- order(apply(ss, 1, function(x)
    dist(rbind(c(median(ss$fN), median(ss$aP)), x))))
  med.ind <- med.order[rks]
  med.sim <- sim.dat[med.ind]
  return(list(ranks = rks, ind = med.ind, dat = med.sim))
}