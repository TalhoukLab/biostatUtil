#' Plot Principal Components
#' 
#' Plot the first three PCs and draw ellipses highlighting
#' differences in the levels of a factor
#' @param by factor to plot as ellipses on PCA plots
#' @param tdat data matrix to compute principal components
#' @return PCA plots for every combination of PC1, PC2, and PC3. The 
#' percentage of variation contribution is shown in the axes labels.
#' @author Aline Talhouk, Derek Chiu
#' @export
#' 
#' @examples 
#' PCAplot(mtcars$cyl, mtcars)
#' PCAplot(mtcars$gear, mtcars)
PCAplot <- function(by, tdat) {
  PC1 <- PC2 <- PC3 <- var.name <- NULL
  p <- prcomp(tdat, retx = TRUE, scale. = TRUE)
  var3 <- round(((p$sdev ^ 2 / sum(p$sdev ^ 2)) * 100)[1:3], 2)
  df <- data.frame(var.name = factor(by),  p$x[, 1:3])
  PCA_geom <- stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.1,
                           size = 0.05, aes(fill = var.name))
  PCA_theme <- theme_bw() + theme(legend.title = element_blank())
  PCA_labs <- sapply(1:3, function(x) paste0("PC", x, " (", var3[x], "% Var)"))
  
  pc1.2 <- qplot(x = PC1, y = PC2, data = df, colour = var.name) +
    PCA_geom + PCA_theme + xlab(PCA_labs[1]) + ylab(PCA_labs[2])
  pc1.3 <- qplot(x = PC1, y = PC3, data = df, colour = var.name) + 
    PCA_geom + PCA_theme + xlab(PCA_labs[1]) + ylab(PCA_labs[3])
  pc2.3 <- qplot(x = PC2, y = PC3, data = df, colour = var.name) + 
    PCA_geom + PCA_theme + xlab(PCA_labs[2]) + ylab(PCA_labs[3])
  
  multiplot(pc1.2, pc1.3, pc2.3, cols = 1)
}