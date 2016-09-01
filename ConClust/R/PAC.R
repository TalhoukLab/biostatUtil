#' Proportion of Ambiguous Clusters
#'
#' Given a consensus matrix, returns the proportion of ambiguous clusters (PAC).
#' This is a robust way to assess clustering performance.
#'
#' Since a consensus matrix is symmetric, we only look at its lower (or upper)
#' triangular matrix. The proportion of entries strictly between \code{lower} and
#' \code{upper} is the PAC. In a perfect clustering, the consensus matrix would
#' consist of only 0s and 1s, and the PAC assessed on the (0, 1) interval would
#' have a perfect score of 0. Using a (0.1, 0.9) interval for defining ambiguity
#' is common as well.
#'
#' @param cons.mat consensus matrix. Should be symmetric and values between 0 and 1.
#' @param lower the lower bound that determines what is ambiguous
#' @param upper the upper bound that determines what is ambiguous
#' @return the PAC is a score used in clustering performance. The lower it is the
#' better, because we want minimal ambiguity amongst the consensus.
#' @author Derek Chiu
#' @importFrom magrittr extract and is_greater_than is_less_than divide_by
#' @import dplyr
#' @export
PAC <- function(cons.mat, lower = 0, upper = 1) {
  . <- NULL
  pac <- cons.mat %>%
    extract(lower.tri(.)) %>%
    extract(and(is_greater_than(., lower), is_less_than(., upper))) %>%
    length() %>%
    divide_by(., length(cons.mat[lower.tri(cons.mat)]))
  return(pac)
}
