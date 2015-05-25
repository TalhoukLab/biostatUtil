source("consensus_clustering/connectivityMatrix.R")
source("consensus_clustering/indicatorMatrix.R")

consensusMatrix <- function(dat) {
  # Given a matrix of replicated NMF clustering assignments
  # Returns the consensus matrix
  require(plyr)
  all.CM <- alply(.data = dat, .margins = 2, .fun = connectivityMatrix)
  all.IM <- alply(.data = dat, .margins = 2, .fun = indicatorMatrix)
  
  sum.CM <- Reduce('+', all.CM)
  sum.IM <- Reduce('+', all.IM)
  
  cons.mat <- Reduce('/', list(sum.CM, sum.IM))
  return(cons.mat)
}