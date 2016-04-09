#' function to plot the first three PCAs and draw ellipses highlighting differences 
#' in the levels of a factor

# @export
PCAplot<-function(by,tdat, mainT = "", sideT = ""){
  require(ggplot2)
  pcaX <- prcomp(tdat,rtx=T,scale.=T)$x
  
  scores<-pcaX[,1:3]
  df <- data.frame(var.name=factor(by),scores)
  
  pc1.2 <- qplot(x=PC1, y=PC2, data=df, colour=var.name)+
    stat_ellipse(geom = "polygon", level=0.9, alpha=0.1,size=0.05, aes(fill = var.name))+
    theme_bw()+theme(legend.title=element_blank())
  
  pc1.3 <- qplot(x=PC1, y=PC3, data=df, colour=var.name) + stat_ellipse(geom = "polygon", level=0.9, alpha=0.1,size=0.05, aes(fill = var.name))+theme_bw()+ theme(legend.title=element_blank())
  
  pc2.3 <- qplot(data=df,x=PC2, y=PC3,colour=var.name) + stat_ellipse(geom = "polygon", level=0.9, alpha=0.1,size=0.05, aes(fill = var.name))+theme_bw()+ theme(legend.title=element_blank())
  
  multiplot(pc1.2, pc1.3, pc2.3, cols=1)
}
