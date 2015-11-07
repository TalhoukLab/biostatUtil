#' Weighted algorithms
#' 
#' Obtain a subset of algorithms weighed by PAC and CHI
#' 
#' @param index.mat matrix of indices for each algorthim outputted
#' from \code{indices}.
#' @param top how many top algorithms to include for weighting. Defaults to 5.
#' @return A matrix of weighted algorithms.
#' @author Derek Chiu
#' @export
weightedAlgs <- function(index.mat, top = 5) {
  Weight.PAC <- Weight.CHI <- CHI <- NULL
  wts <- index.mat %>%
    arrange(desc(CHI)) %>% 
    head(top) %>% 
    mutate(Weight.PAC = (1 - PAC) / sum((1 - PAC)),
           Weight.CHI = CHI / sum(CHI),
           Weight = (Weight.PAC + Weight.CHI) / 2)
  return(wts)
}