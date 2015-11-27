
PCAplot<-function(batch,pcaX,mainT,sideT){
  scores<-pcaX[,1:3]
  df <- data.frame(By=factor(batch),scores)
  pc1.2 <- qplot(x=PC1, y=PC2, data=df, colour=By)+
    stat_ellipse(geom = "polygon", level=0.9, alpha=0.1,size=0.05, 
                 aes(fill = By))+
    theme_bw()
  pc1.3 <- qplot(x=PC1, y=PC3, data=df, colour=By) + stat_ellipse(geom = "polygon", level=0.9, alpha=0.1,size=0.05, aes(fill = By))+theme_bw()
  pc2.3 <- qplot(data=df,x=PC2, y=PC3,colour=By) + stat_ellipse(geom = "polygon", level=0.9, alpha=0.1,size=0.05, aes(fill = By))+theme_bw()
  multiplot(pc1.2, pc1.3, pc2.3, cols=1)
  
}

