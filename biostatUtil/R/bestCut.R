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
#' @author Derek Chiu
#' @importFrom stats AIC logLik
#' @importFrom broom glance
#' @export
bestCut <- function(f, d, AIC.range = 3, plot = TRUE, save = TRUE,
                    filename = NULL, nrow = NULL, ncol = NULL, title = "",
                    ...) {
  . <- cutpoints <- p.value.log <- NULL
  pos <- 1
  assign("f", f, envir = as.environment(pos))
  assign("d", d, envir = as.environment(pos))
  term <- all.vars(f)[3]
  levs <- sort(unique(d[, term]))
  cuts <- levs[-length(levs)]
  bins <- sapply(cuts, function(x) 
    ifelse(d[, term] <= x, paste("At Most", x), paste("Over", x)))
  coxs <- apply(bins, 2, function(bin) 
    coxph(as.formula(paste(deparse(f[[2]]), "~ bin")), d))
  diffs <- apply(bins, 2, function(bin)
    survfit(as.formula(paste(deparse(f[[2]]), "~ bin")), d))
  results <- coxs %>% 
    sapply(., broom::glance) %>% 
    rbind(cutpoints = cuts, .) %>% 
    t() %>%
    data.frame() %>% 
    dplyr::select(cutpoints, p.value.log, logLik, AIC)
  AIC.vals <- unlist(results$AIC)
  AIC.lowest <- which.min(AIC.vals)
  
  # Cutpoint that halves the group size and number of events best
  half.ind <- sapply(diffs, function(x) summary(x)$table[, "events"]) %>%
    prop.table(2) %>% 
    apply(., 2, prod) %>% 
    which.max()
  
  # Check for flat likelihood issue using range of AIC
  if (diff(range(results$AIC)) < AIC.range) {
    opt.ind <- half.ind
  } else {
    opt.ind <- which.min(results$AIC)
  }
  opt.cut <- results$cutpoints[[opt.ind]]
  
  # Plot survival curves for every cutpoint in PNG file
  if (plot) {
    if (save) {
      png(filename, width = 8.5, height = 11, units = "in", res = 300)
      par(mfrow = c(nrow, ncol))
      mapply(function(x, y) {
        plot(x, main = paste(title, y), col = 1:2, lwd = 1, ...)
        legend("bottomleft", legend = stringr::str_split_fixed(
          names(x$strata),"=", 2)[, 2], col = 1:2, lwd = 1, cex = 0.75)},
        diffs, cuts)
      dev.off()
      par(mfrow = c(1, 1))
    } else {
      par(mfrow = c(nrow, ncol))
      mapply(function(x, y) {
        plot(x, main = paste(title, y), col = 1:2, lwd = 1, ...)
        legend("bottomleft", legend = stringr::str_split_fixed(
          names(x$strata), "=", 2)[, 2], col = 1:2, lwd = 1, cex = 0.75)},
        diffs, cuts)
      par(mfrow = c(1, 1))
    }
  }
  return(list(cuts = cuts, fits = coxs, results = results, opt.cut = opt.cut))
}
