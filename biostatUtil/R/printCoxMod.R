#' Print Cox model output
printCoxMod<-function(cox,Capt){
  TAB=htmlTable(cox, 
                rowlabel="Predictors", 
                #rgroupCSSstyle="", rgroupCSSseparator="", 
                caption=Capt, 
                ctable=TRUE)
  pander(TAB, style='rmarkdown')
}