
PAC <- function(cons.mat, lower = 0, upper = 1) {
  # Given a consensus matrix, lower bound, and upper bound,
  # Returns the Proportion of Ambiguous Clusters
  require(dplyr)
  require(magrittr)
  pac <- cons.mat %>%
    extract(lower.tri(.)) %>%
    extract(and(is_greater_than(., lower), is_less_than(., upper))) %>%
    length %>%
    divide_by(., length(cons.mat[lower.tri(cons.mat)]))
  return(pac)
}