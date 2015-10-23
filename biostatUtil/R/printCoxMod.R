#' Print Cox model output
#' 
#' Prints a Cox model output in a nice HTML table
#' @param cox an object of class \code{coxph}
#' @param Capt a caption
#' @author Aline Talhouk
#' @export
printCoxMod <- function(cox, Capt) {
  TAB <- htmlTable::htmlTable(cox, 
                              rowlabel = "Predictors", 
                              caption = Capt, 
                              ctable = TRUE)
  pander::pander(TAB, style = 'rmarkdown')
}