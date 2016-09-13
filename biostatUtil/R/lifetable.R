#' Generate lifetables for multiclass variables
#' 
#' Specify vector of time endpoints and create a cohort life table for two or 
#' more strata
#' 
#' Essentially a wrapper around \code{\link[KMsurv]{lifetab}} that allows the
#' user to input a \code{survfit} object instead of vectors of raw values.
#' 
#' @param obj An object of class \code{survfit}
#' @param ntimes number of time intervals
#' @param times A vector of endpoints of time intervals to show life table 
#'   calculations. By default, these are \code{ntimes} evenly spaced out 
#'   endpoints based on the full range of survival times.
#' @param nround number of digits to round table values
#' @param show.strata logical; if \code{TRUE} (default), the variable name is 
#'   appended to the beginning of each stratum in the lifetable's \code{strata} 
#'   column
#' @param strata.name column name for the different strata
#' @param summary logical; if \code{TRUE}, a case processing summary is shown 
#'   with number of subjects, events, censored, and percent censored per 
#'   stratum.
#' @return A table with the following columns:
#' \item{strata}{name of specific group in variable}
#' \item{times}{time interval}
#' \item{nsubs}{See \code{\link[KMsurv]{lifetab}}}
#' \item{nlost}{See \code{\link[KMsurv]{lifetab}}}
#' \item{nrisk}{See \code{\link[KMsurv]{lifetab}}}
#' \item{nevent}{See \code{\link[KMsurv]{lifetab}}}
#' \item{surv}{See \code{\link[KMsurv]{lifetab}}}
#' \item{pdf}{See \code{\link[KMsurv]{lifetab}}}
#' \item{hazard}{See \code{\link[KMsurv]{lifetab}}}
#' \item{se.surv}{See \code{\link[KMsurv]{lifetab}}}
#' \item{se.pdf}{See \code{\link[KMsurv]{lifetab}}}
#' \item{se.hazard}{See \code{\link[KMsurv]{lifetab}}}
#' @author Derek Chiu
#' @seealso \code{\link[KMsurv]{lifetab}}
#' @export
#' @examples
#' library(survival)
#' obj <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
#' lifetable(obj)
#' lifetable(obj, ntimes = 4, show.strata = FALSE)
#' lifetable(obj, ntimes = 4, times = c(200, 500, 800, 1000))
lifetable <- function(obj, ntimes = 3, times = NULL, nround = 3,
                      show.strata = TRUE, strata.name = "strata",
                      summary = FALSE) {
  nevent <- nlost <- nsubs <- plost <- NULL
  cuts <- cumsum(obj$strata)
  if (is.null(times)) {
    times <- round(quantile(obj$time, 1 / ntimes * rep(1:ntimes)))
  }
  if (ntimes > 1) {
    ind <- sapply(split_pos(obj$time, cuts), function(x) {
      mat <- abs(sapply(times, "-", x)) %>% 
        apply(., 2, which.min)
      return(mat)
    })
    ind[ntimes, ] <- obj$strata
    ind <- as.list(as.data.frame(ind))
  } else {
    ind <- obj$strata
  }
  cs <- mapply(function(x, ind) sapply(split_pos(x, ind), sum)[-(length(ind) + 1)],
               split_pos(obj$n.censor, cuts), ind, SIMPLIFY = FALSE)
  es <- mapply(function(x, ind) sapply(split_pos(x, ind), sum)[-(length(ind) + 1)],
               split_pos(obj$n.event, cuts), ind, SIMPLIFY = FALSE)
  if (show.strata)
    strata <- names(obj$strata)
  else
    strata <- gsub(".+=", "\\1", names(obj$strata))
  tab <- mapply(function(n, c, e) KMsurv::lifetab(c(0, times), n, c, e),
                obj$n, cs, es, SIMPLIFY = FALSE) %>% 
    lapply(round, nround) %>% 
    plyr::ldply(function(.) cbind(times = rownames(.), .)) %>% 
    cbind(strata = rep(strata, each = ntimes), .)
  if (summary) {
    tab <- tab %>% 
      select(strata, nsubs, nevent, nlost) %>%
      mutate(strata = as.character(strata)) %>%
      rbind(c("Overall", colSums(.[-1]))) %>%
      mutate_each_(funs(as.numeric), names(.)[-1]) %>%
      mutate(plost = paste0(sprintf("%.1f", nlost / nsubs * 100), "%")) %>%
      dplyr::rename(`Total N` = nsubs, `N of Events` = nevent,
                    `N of Censored` = nlost, `Percent Censored` = plost)
  }
  colnames(tab)[1] <- eval(strata.name)
  return(tab)
}

#' Split vector at a position for calculating cumulative sums
#' @noRd
split_pos <- function(x, pos) {
  unname(split(x, cumsum(seq_along(x) %in% (pos + 1))))
}