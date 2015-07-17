#' Generate a legend
#' 
#' Given a ggplot object, generates a legend
#' 
#' @param a.gplot ggplot object
#' @return ggplot object with legend
#' @author Aline Talhouk
#' @export
g_legend <- function(a.gplot) {
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}