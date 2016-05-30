#' Select pcNormal simulated datasets
#'
#' @param sim.dat a list object from \code{pcNormal}
#' @param cl cluster memberships
#' @param type select either the representative dataset or a range of datasets
#' @param int every \code{int} data sets from median ranked \code{sim.dat}
#' are taken. Takes every 5th dataset by default.
#' @return A list with elements
#' \item{ranks}{When \code{type = "range"}, ranks of each extracted dataset shown}
#' \item{ind}{index of representative simulation}
#' \item{dat}{simulation data representation of all in pcNormal}
#' @author Derek Chiu
#' @importFrom stats median
#' @export
pcNormalSelect <- function(sim.dat, cl, type = c("rep", "range"), int = 5) {
  ss <- plyr::ldply(sim.dat, silStats, cl)
  type <- match.arg(type)
  switch(type,
         rep = {
           ind <- which.min(apply(ss, 1, function(x)
             dist(rbind(c(median(ss$fN), median(ss$aP)), x))))
           sim <- sim.dat[[ind]]
           return(list(ind = ind, dat = sim))
         },
         range = {
           rks <- seq(1 + int, length(sim.dat), int)
           ord <- order(apply(ss, 1, function(x)
             dist(rbind(c(median(ss$fN), median(ss$aP)), x))))
           ind <- ord[rks]
           sim <- sim.dat[ind]
           return(list(ranks = rks, ind = ind, dat = sim))
         })
}
