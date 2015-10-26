#' Generate cohort characteristics
#' @param input.d The \code{data.frame} containing the data
#' @param marker.name The variable that you want to split into different columns
#' @param marker.description The description for the variable(s) to split
#' @param var.names The variables that you want the statistics for
#' @author Aline Talhouk
#' @export
#' 
NumericStatsby <- function (input.d, by1, by2,
                                    marker.description=by1,
                                    groups.description=by2,
                                    var.names,  
                                    var.descriptions=var.names,
                                    digits=1,
                                    missing.codes.highlight = NULL,
                                    missing.codes = c("N/A", "", "Unk"),
                                    decimal = 0, caption = NA) {

library(doBy)
library(pander)
summary.table <- NULL
varby1 <- input.d[,by1]
varby2 <- input.d[,by2]

var.name=var.names[i]
t.dat <-input.d[,c(var.name, by1,by2)]
colnames(t.dat) <- c("var", "varby1", "varby2")
type=class(t.dat[,"var"])
contSumFunc <- function(x) {
  round(c(mean = mean(x, na.rm=T), s = sd(x, na.rm=T), 
          median=median(x, na.rm=T),range=range(x, na.rm=T), missing=sum(is.na(x))),digits) }
if(type=="numeric"){
values <- summaryBy(var~varby1 + varby2, data=t.dat, FUN = contSumFunc )
totalvarby1 <- summaryBy(var~varby1, data=t.dat, FUN = contSumFunc )
totalvarby2<- summaryBy(var~varby2, data=t.dat,FUN = contSumFunc)
total <- c("Total",contSumFunc(t.dat[,"var"]))

#Assemble the table
main=t(cbind(c(sapply(seq_len(length(levels(varby1))), 
        function(i) c(levels(varby1)[i], rep(" ", each=length(levels(varby2))-1)))),
           rep(levels(varby2), length(levels(varby1))),
           ms =paste(values[,"var.mean"],values[,"var.s"],sep="&#177; "),
           med = values[,"var.median"], 
           r= paste(values[,"var.range1"],values[,"var.range2"],sep="-"),
           missing= values[,"var.missing"]))

rownames(main)= c(" "," ","mean","median","range", "missing")
}
else{
  
}




pandoc.table(main, justify='left', style="simple")

