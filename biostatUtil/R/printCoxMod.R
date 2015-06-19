#' Print Cox model output
#' @export
printCoxMod <- function(cox, Capt) {
  TAB <- htmlTable(cox, 
                rowlabel = "Predictors", 
                #rgroupCSSstyle="", rgroupCSSseparator="", 
                caption = Capt, 
                ctable = TRUE)
  pander::pander(TAB, style = 'rmarkdown')
}