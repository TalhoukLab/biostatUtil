#' QC for NanoString Data
#'
#' This function computes NanoString quality control parameters and flags
#' @param raw: matrix of raw counts obtained from nCounter (rows are genes). The first three columns must be labeled: c("Code.Class","Name","Accession") and contain that information.
#' @param exp:  matrix of annotations with rows in the same order as the columns of raw.
#' @param detect: Percent threshold of genes over lod that we would like to detect (not decimal).
#' @param sn: Signal to noise ratio of the housekeeping genes we are willing to tolerate
#' @param plots: Binary, indicates whether plots to visualise the results are requested, defaults to TRUE
#' @param ttl: a string to show the title on the plots
#' @param explore: returns the plots only, defaults to TRUE
#' @return matrix of annotations updated with normalization parameters


NanoStringQC <- function(raw, exp, detect = 80, sn = 150, plots = TRUE, ttl =" ", explore = TRUE){
  genes = raw$Name
  rownames(raw) = genes
  HKgenes = genes[raw$Code.Class == "Housekeeping"]
  PCgenes = genes[raw$Code.Class == "Positive"]
  NCgenes = genes[raw$Code.Class == "Negative"]
  PCconc = sub("\\).*", "", sub(".*\\(", "", PCgenes))
  PCconc = as.numeric(PCconc)
  lin = function(x){
    if (any(x == 0)){
      res = NA
    } else {
      fit <- lm(x ~ PCconc)
      res = summary(fit)$r.squared }
    return(res)
  }

  exp$linPC <- apply(raw[PCgenes, -(1:3)], 2, lin)
  exp$linFlag <- factor(ifelse(exp$linPC < 0.95 | is.na(exp$linPC),"Failed","Passed"), levels = c("Failed","Passed"))
  exp$perFOV <- (exp$fov.counted / exp$fov.count) * 100
  exp$imagingFlag <- factor(ifelse(exp$perFOV < 75,"Failed","Passed"), levels = c("Failed","Passed"))
  exp$lod <- apply(raw[NCgenes, -(1:3)], 2, mean) + 2 * (apply(raw[NCgenes, -(1:3)], 2, sd))
  exp$llod <- apply(raw[NCgenes, -(1:3)], 2, mean) - 2 * (apply(raw[NCgenes, -(1:3)], 2, sd))
  exp$lodFlag <- factor(ifelse(t(as.vector(raw["POS_E(0.5)", -(1:3)]) < exp$lod),"Failed","Passed"), levels = c("Failed","Passed"))
  exp$gd <- apply(raw[, -(1:3)] > exp$lod, 2, sum) # Number of genes above background
  exp$pergd <- (exp$gd / nrow(raw)) * 100
  exp$averageHK <- exp(apply(log(raw[HKgenes, -(1:3)], base = 2), 2, mean)) # Geometric Mean
  exp$sn <- exp$averageHK / exp$lod
  exp$sn[exp$lod < 0.001] <- 0 # This is so that it does not underflow
  exp$bdFlag <- factor(ifelse(exp$binding.density < 0.05 | exp$binding.density > 2.25,"Failed","Passed"), levels = c("Failed","Passed"))

  if (plots==TRUE) {
    par(mfrow = c(1,2))
    plot(exp$sn, exp$pergd, pch=20, col="deepskyblue", xaxt="n", ylim=c(0,100),
         xlab = "Signal to Noise Ratio", ylab = "Ratio of Genes Detected")
    axis(1, at = seq(0, max(exp$sn) + 1, 300))
    abline(v = sn, col = "red", lwd = 2)
    abline(h = detect, lty = 2)

    hist(exp$sn, 50, col = "cornsilk", prob = TRUE,
         xlab = "Signal to Noise", ylab = "Probability", main = "")
    abline(v = sn,lwd = 3)
    title(ttl, outer = TRUE, line = -2)
  }
  exp$normFlag <- factor(ifelse(exp$sn < sn | exp$pergd < detect,"Failed","Passed"), levels = c("Failed","Passed"))
  exp$QCFlag <- as.vector(exp$lodFlag == TRUE | exp$normFlag == TRUE | exp$imagingFlag == TRUE | exp$linFlag == TRUE)
  if (!explore)
    return(exp$QCFlag)
  else
    return(exp)
}
