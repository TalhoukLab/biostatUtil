#' function to plot the first three PCAs and draw ellipses highlighting differences in the levels of a factor
#' @param by factor to plot as ellipses on PCA plots
#' @param tdat data matrix to compute principal componentsT
#' @importFrom stats prcomp
#' @export
PCAplot <- function(by, tdat) {
  PC1 <- PC2 <- PC3 <- var.name <- NULL
  p <- prcomp(tdat, rtx = TRUE, scale. = TRUE)
  var3 <- round(((p$sdev ^ 2 / sum(p$sdev ^ 2)) * 100)[1:3], 2)
  pcaX <- p$x
  scores <- pcaX[, 1:3]
  df <- data.frame(var.name = factor(by), scores)
  
  pc1.2 <- qplot(x = PC1, y = PC2, data = df, colour = var.name) +
    stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.1, size = 0.05,
                 aes(fill = var.name)) +
    theme_bw() + theme(legend.title = element_blank()) +
    xlab(paste0("PC1 (", var3[1], "% Var)")) +
    ylab(paste0("PC2 (", var3[2], "%Var)"))
  
  pc1.3 <- qplot(x = PC1, y = PC3, data = df, colour = var.name) + 
    stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.1, size = 0.05,
                 aes(fill = var.name)) +
    theme_bw() + theme(legend.title = element_blank()) + 
    xlab(paste0("PC1 (", var3[1], "% Var)")) + 
    ylab(paste0("PC3 (", var3[3], "%Var)"))
  
  pc2.3 <- qplot(data = df, x = PC2, y = PC3, colour = var.name) + 
    stat_ellipse(geom = "polygon", level = 0.9, alpha = 0.1, size = 0.05,
                 aes(fill = var.name)) + 
    theme_bw() + theme(legend.title = element_blank()) +
    xlab(paste0("PC2 (", var3[3], "% Var)")) +
    ylab(paste0("PC3 (", var3[3], "%Var)"))
  
  multiplot(pc1.2, pc1.3, pc2.3, cols = 1)
}
