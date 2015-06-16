#' Consolidate POLE germline to UNK
#' @export
consolidateGermline <- function(x){
  return (ifelse(
    x %in% c(ALL.MISSING.CODES,POLE.GERMLINE.FLAG), 
    MISSING.BIOMARKER.EXPLICIT, 
    ifelse(x==0,VALUE.CODING.WILD.TYPE,VALUE.CODING.MUTATED))
  )
}