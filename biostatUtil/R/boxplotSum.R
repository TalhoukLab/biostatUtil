#' Annotated Boxplot
#'
#' Function to provide a boxplot annotated with a 5 point summary.
#' @param var the variable to be plotted (must be numerical)
#' @param ttl the title of the boxplot
#' @param digit the number of digits used for rounding (defaults to 1)
#' @author Aline Talhouk
#' @export
#'
boxplotSum<-function(var,ttl,digit=1){
  bxp <- boxplot(var, col="lightgrey", border = "darkgrey",
                 horizontal=TRUE, axes=FALSE, main=ttl)
  mtext(c("","Q1","Med","Q3",""), side=3, at=bxp$stats, line=-2,cex=0.8)
  mtext(c(round(bxp$stats[1],digit),round(bxp$stats[2],digit),round(bxp$stats[3],digit),round(bxp$stats[4],digit),round(bxp$stats[5],digit)), side=3, at=bxp$stats, line=-8,cex=0.8)
}
