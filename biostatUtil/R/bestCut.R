#' Find the best cutpoint for a covariate
#' 
#' Repeatedly finds cutpoints for an explanatory variable in a
#' univariable Cox PH model. Also plots survival curves for each 
#' cutpoint.
#' 
#' Takes the cutpoint resulting in the lowest AIC. If the range of AIC values is 
#' within \code{AIC.range} units, take the cutpoint that results in the
#' two groups having the most similar numbers of events and cases
#'
#' @param f formula object
#' @param d data frame
#' @param AIC.range If range of AIC is within \code{AIC.range} units, the
#' likelihood is too flat. We choose the best cutpoint using the alternative method.
#' @param plot logical; If \code{TRUE}, shows the survival curves for each cutpoint
#' in a facetted figure
#' @param save logical; If \code{TRUE}, saves the plot as a png image
#' @param filename file name for png image of figure
#' @param nrow number of rows in facetted plot
#' @param ncol number of columns in facetted plot
#' @param title title for plot
#' @param ... additional arguments to \code{plot}
#'
#' @return A list with the following elements
#' \item{cuts}{vector of cutpoints considered}
#' \item{fits}{A list of \code{coxph} objects run for each cutpoint}
#' \item{results}{A table showing the likelihood ratio test p-value,
#' log likelihood, and AIC for each cutpoint}
#' \item{opt.cut}{optimal cutpoint value}
#' \item{flat.lik}{If \code{TRUE}, the likelihood was too flat and the
#' alternative method was used}
#' @author Derek Chiu
#' @importFrom stats AIC logLik
#' @importFrom broom glance
#' @export
bestCut <- function(f, d, n = c("b", "t", "q"), AIC.range = 3, nround = 3,
                    plot = TRUE, save = TRUE, filename = NULL, nrow = NULL,
                    ncol = NULL, title = "", lwd = 1, cex = 0.75, ...) {
  . <- cutpoints <- p.value.log <- NULL
  pos <- 1
  ncolor <- switch(n, b = 2, t = 3, q = 4)
  assign("f", f, envir = as.environment(pos))
  assign("d", d, envir = as.environment(pos))
  term <- all.vars(f)[3]
  levs <- sort(unique(d[, term]))
  bins <- build_cuts(d[, term], n = n) 
  cuts <- stringr::str_extract_all(names(bins), ".v", simplify = TRUE) %>% 
    apply(., 1, paste, collapse = ", ") %>% 
    gsub("v", "", .)
  coxs <- lapply(as.list(bins), function(bin) 
    coxph(as.formula(paste(deparse(f[[2]]), "~ bin")), d))
  diffs <- lapply(as.list(bins), function(bin)
    survfit(as.formula(paste(deparse(f[[2]]), "~ bin")), d))
  results <- coxs %>% 
    sapply(., broom::glance) %>% 
    rbind(cutpoints = cuts, .) %>% 
    t() %>%
    data.frame() %>% 
    dplyr::select(cutpoints, p.value.log, logLik, AIC)
  p.vals <- signif(unlist(results$p.value.log), nround)
  AIC.vals <- round(unlist(results$AIC), nround)
  AIC.lowest <- which.min(AIC.vals)
  
  # Check for flat likelihood issue using range of AIC
  if (diff(range(results$AIC)) < AIC.range) {
    opt.ind <- sapply(diffs, function(x) summary(x)$table[, "events"]) %>%
      prop.table(2) %>% 
      apply(., 2, prod) %>% 
      which.max()  # Cutpoint that halves the group size and number of events best
    flat.lik <- TRUE
  } else {
    opt.ind <- which.min(results$AIC)
    flat.lik <- FALSE
  }
  opt.cut <- results$cutpoints[[opt.ind]]
  best.ind <- rep("", length(cuts)) %>% 
    magrittr::inset(opt.ind, "(Best)")
  # title.range <- cutRange(levs)
  title.range <- names(bins)
  
  # Plot survival curves for every cutpoint in PNG file
  if (plot) {
    if (save) {
      png(filename, width = 8.5, height = 11, units = "in", res = 300)
      par(mfrow = c(nrow, ncol))
      mapply(function(x, y, z, aic, pval) {
        plot(x, main = paste(title, y, z), col = 1:ncolor, lwd = lwd, ...)
        legend("bottomleft", legend = stringr::str_split_fixed(
          names(x$strata), "=", 2)[, 2], col = 1:ncolor, lwd = lwd, cex = cex)
        mtext(paste("P =", pval), side = 1, line = -3, at = max(x$time), adj = 1, cex = cex)
        mtext(paste("AIC:", aic), side = 1, line = -1, at = max(x$time), adj = 1, cex = cex)
      }, diffs, title.range, best.ind, AIC.vals, p.vals)
      dev.off()
      par(mfrow = c(1, 1))
    } else {
      par(mfrow = c(nrow, ncol))
      mapply(function(x, y, z, aic, pval) {
        plot(x, main = paste(title, y, z), col = 1:ncolor, lwd = lwd, ...)
        legend("bottomleft", legend = stringr::str_split_fixed(
          names(x$strata), "=", 2)[, 2], col = 1:ncolor, lwd = lwd, cex = cex)
        mtext(paste("P =", pval), side = 1, line = -3, at = max(x$time), adj = 1, cex = cex)
        mtext(paste("AIC:", aic), side = 1, line = -1, at = max(x$time), adj = 1, cex = cex)
      }, diffs, title.range, best.ind, AIC.vals, p.vals)
      par(mfrow = c(1, 1))
    }
  }
  return(list(cuts = cuts, fits = coxs, results = results, opt.cut = opt.cut,
              flat.lik = flat.lik))
}
