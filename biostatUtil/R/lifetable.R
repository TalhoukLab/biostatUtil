#' Generate lifetables for multiclass variables
#' 
#' Specify vector of time endpoints and create a cohort life table for two or more strata
#' 
#' Essentially a wrapper around \code{lifetab} that allows the user to input a \code{survfit}
#' object instead of vectors of raw values.
#'
#' @param obj An object of class \code{survfit}
#' @param ntimes number of time intervals
#' @param times A vector of endpoints of time intervals to show life table calculations.
#' By default, these are \code{ntimes} evenly spaced out endpoints based on the full range
#' of survival times.
#' @param nround number of digits to round table values
#' @param var.name logical; if \code{TRUE} (default), the variable name is appended to the
#' beginning of each stratum in the lifetable's \code{strata} column
#'
#' @return A table with the following columns:
#' \item{strata}{name of specific group in variable}
#' \item{times}{time interval}
#' \item{nsubs}{See \code{\link{lifetab}}}
#' \item{nlost}{See \code{\link{lifetab}}}
#' \item{nrisk}{See \code{\link{lifetab}}}
#' \item{nevent}{See \code{\link{lifetab}}}
#' \item{surv}{See \code{\link{lifetab}}}
#' \item{pdf}{See \code{\link{lifetab}}}
#' \item{hazard}{See \code{\link{lifetab}}}
#' \item{se.surv}{See \code{\link{lifetab}}}
#' \item{se.pdf}{See \code{\link{lifetab}}}
#' \item{se.hazard}{See \code{\link{lifetab}}}
#' @author Derek Chiu
#' @seealso \code{\link{lifetab}}
#' @importFrom KMsurv lifetab
#' @export
#'
#' @examples
#' obj <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
#' lifetable(obj)
#' lifetable(obj, ntimes = 4, var.name = FALSE)
#' lifetable(obj, ntimes = 4, times = c(200, 500, 800, 1000))
lifetable <- function(obj, ntimes = 3,
                      times = round(quantile(obj$time, 1/ntimes * rep(1:ntimes))),
                      nround = 3, var.name = TRUE) {
  . <- NULL
  cuts <- cumsum(obj$strata)
  if (ntimes > 1) {
    ind <- sapply(splitAt(obj$time, cuts), function(x) {
      mat <- abs(sapply(times, "-", x)) %>% 
        apply(., 2, which.min)
      return(mat)
    })
    ind[ntimes, ] <- obj$strata
    ind <- as.list(as.data.frame(ind))
  } else {
    ind <- obj$strata
  }
  cs <- mapply(function(x, ind) sapply(splitAt(x, ind), sum)[-(length(ind) + 1)],
               splitAt(obj$n.censor, cuts), ind, SIMPLIFY = FALSE)
  es <- mapply(function(x, ind) sapply(splitAt(x, ind), sum)[-(length(ind) + 1)],
               splitAt(obj$n.event, cuts), ind, SIMPLIFY = FALSE)
  if (var.name)
    strata <- names(obj$strata)
  else
    strata <- gsub(".+=", "\\1", names(obj$strata))
  tab <- mapply(function(n, c, e) KMsurv::lifetab(c(0, times), n, c, e),
                obj$n, cs, es, SIMPLIFY = FALSE) %>% 
    lapply(round, nround) %>% 
    plyr::ldply(function(.) cbind(times = rownames(.), .)) %>% 
    cbind(strata = rep(strata, each = ntimes), .)
  return(tab)
}
