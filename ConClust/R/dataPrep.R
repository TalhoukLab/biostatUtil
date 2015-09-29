#' Prepare data for consensus clustering
#'
#' Remove variables with low signal and scale before consensus clustering
#' (using \code{ConClust})
#' 
#' The \code{min.sd} argument is used to filter the feature space for only highly variable
#' features. Only features with a standard deviation across all samples greater than
#' \code{min.sd} will be used.
#'
#' @param data data matrix. Columns are samples and rows are genes/features.
#' @param min.sd minimum standard deviation threshold. See details.
#' @return dataset prepared for usage in \code{ConClust}
#' @export
dataPrep <- function(data, min.sd = 1) {
  . <- NULL
  dat.out <- data %>%
    as.data.frame %>%
    select(which(sapply(., class) == "numeric")) %>%
    extract(apply(., 1, function(x) sd(x, na.rm = T)) > min.sd,
            apply(., 2, function(x) !any(is.na(x)))) %>%
    t %>%
    scale %>%
    t
  return(dat.out)
}
