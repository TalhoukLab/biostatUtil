# Plotting function for bestCut
bestCut_plot <- function(x, title, pval = NULL, aic = NULL, lwd = 1, cex = 0.75, ...) {
  plot(x, main = title, col = 1:length(x$strata), lwd = lwd, ...)
  legend("bottomleft", legend = stringr::str_split_fixed(
    names(x$strata), "=", 2)[, 2], col = 1:length(x$strata), lwd = lwd, cex = cex)
  if (!is.null(pval))
    mtext(paste("P =", pval), side = 1, line = -2, at = max(x$time), adj = 1, cex = cex)
  if (!is.null(aic))
    mtext(paste("AIC:", aic), side = 1, line = -1, at = max(x$time), adj = 1, cex = cex)
}