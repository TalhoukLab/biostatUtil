#' Annotated Barplot
#'
#' Function to provide a barplot from a table of categorical variable with labels wraped
#' @param var the variable to be plotted (must be categorical)
#' @param ttl the title of the boxplot
#' @author Aline Talhouk
#' @export
#'
barplotSum <- function(tx, ttl=""){
wrap.labels <- function(x, len)
{wrap.it <- function(x, len)
  {
  sapply(x, function(y) paste(strwrap(y, len),
                              collapse = "\n"),
         USE.NAMES = FALSE)
  }
  if (is.list(x))
  {
    lapply(x, wrap.it, len)
  } else {
    wrap.it(x, len)
  }
}
wr.lap <- wrap.labels(names(tx), 35)

barplot(prop.table(tx)*100, border = "white",
        horiz = T, las=2,names.arg = wr.lap, offset = 0,
        main=ttl, xlab = "%", cex.names = 0.5, col="lightblue")
}
