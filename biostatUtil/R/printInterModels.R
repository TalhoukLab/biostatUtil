#' Print inter models
#' @export
printInterModels<- function(mod1,mod2,mod3,Capt){
  vars <- rbind(mod1,mod2,mod3)
  
  cgroup <- c("Coefficients","HR and [95% CI]")
  n.cgroup <- c(4,3)
  rgroup <- c("Model 1", "Model 2", "Model 3")
  n.rgroup <- c(nrow(mod1), nrow(mod2), nrow(mod3))
  TAB=htmlTable(vars, 
                rowlabel="Models Considered", 
                rgroup=rgroup, n.rgroup = n.rgroup, 
                #rgroupCSSstyle="", rgroupCSSseparator="", 
                caption=paste(Capt,"Models <sup>&dagger;</sup> testing for interactions",sep=""), 
                tfoot="<sup>&dagger;</sup> Model 1 includes POLE only, Model 2 includes POLE adjusted for treatment, and Model 3 includes POLE, treatment and an interaction",
                ctable=TRUE)
  pander(TAB, style='rmarkdown')
  
  
  
}