#' Multiple plots
#'
#' Place multiple ggplot objects on the same figure space
#' 
#' If the \code{layout} is something like \code{matrix(c(1, 2, 3, 3),
#' nrow = 2, byrow = TRUE)}, then plot 1 will go in the upper left, 2 will
#' go in the upper right, and 3 will go all the way across the bottom.
#'
#' @param ... pass ggplot objects
#' @param plotlist pass a list of ggplot objects
#' @param cols number of columns in layout
#' @param layout a matrix specifying the layout.
#' If present, \code{cols} is ignored.
#' @return A grid object made up of multiple ggplots
#' @author Aline Talhouk
#' @export
multiplot <- function(..., plotlist = NULL, cols = 1, layout = NULL) {
  plots <- c(list(...), plotlist)
  numPlots <- length(plots)
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots / cols)),
                     ncol = cols, nrow = ceiling(numPlots / cols))
  }
  if (numPlots == 1) {
    print(plots[[1]])
  } else {
    grid::grid.newpage()
    grid::pushViewport(grid::viewport(layout = grid.layout(nrow(layout),
                                                           ncol(layout))))
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}