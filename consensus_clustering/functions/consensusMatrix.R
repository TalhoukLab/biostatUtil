source("consensus_clustering/functions/connectivityMatrix.R")
source("consensus_clustering/functions/indicatorMatrix.R")

consensusMatrix <- function(dat) {
  # Input matrix has rows as samples, columns as reps
  # Returns the consensus matrix
  require(plyr)
  all.CM <- alply(dat, 2, connectivityMatrix)
  all.IM <- alply(dat, 2, indicatorMatrix)
  
  sum.CM <- Reduce('+', all.CM)
  sum.IM <- Reduce('+', all.IM)
  
  cons.mat <- Reduce('/', list(sum.CM, sum.IM))
  return(cons.mat)
}