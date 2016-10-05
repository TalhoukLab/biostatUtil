#' Annotated Barplot
#' 
#' Function to provide a barplot from a table of categorical variable with
#' labels wrapped
#' @param tx table of a categorical variable
#' @param ttl the title of the boxplot
#' @author Aline Talhouk
#' @export
barplotSum <- function(tx, ttl = "") {
  wr.lap <- wrap.labels(names(tx), 35)
  barplot(prop.table(tx) * 100, border = "white",
          horiz = TRUE, las = 2, names.arg = wr.lap, offset = 0,
          main = ttl, xlab = "%", cex.names = 0.5, col = "lightblue")
}

#' Wrap labels
#' @noRd
wrap.labels <- function(x, len) {
  return(sapply(x, function(y) paste(strwrap(y, len), collapse = "\n"),
                USE.NAMES = FALSE))
}

#' Annotated Boxplot
#'
#' Function to provide a boxplot annotated with a 5 point summary.
#' @param var the variable to be plotted (must be numerical)
#' @param ttl the title of the boxplot
#' @param digit the number of digits used for rounding (defaults to 1)
#' @author Aline Talhouk
#' @export
boxplotSum <- function(var, ttl, digit = 1) {
  bxp <- boxplot(var, col = "lightgrey", border = "darkgrey",
                 horizontal = TRUE, axes = FALSE, main = ttl)
  mtext(c("", "Q1", "Med", "Q3", ""), side = 3, at = bxp$stats,
        line = -2, cex = 0.8)
  mtext(c(round(bxp$stats[1], digit), round(bxp$stats[2], digit),
          round(bxp$stats[3], digit), round(bxp$stats[4], digit),
          round(bxp$stats[5], digit)), side = 3, at = bxp$stats,
        line = -8, cex = 0.8)
}

#' Summary histogram
#' 
#' Function to create an annotated histogram, with density plot
#' @param var  a variable to be plotted
#' @param xlab X axis label
#' @param txt  text to be used in the title (defaults to none)
#' @param sub  a sublabel (defaults to none)
#' @param digit number of digits used in rounding (defaults to 1)
#' @author Aline Talhouk
#' @export
histSum <- function(var, xlab = "", txt = "", sub = "", digit = 1) {
  h <- hist(var, main = paste("Mean", round(mean(var, na.rm = TRUE), digit),
                              "SD", round(sd(var, na.rm = TRUE), digit),
                              "Missing", sum(is.na(var))), prob = F, xlab = xlab,
            col = "white", border = "grey", sub = sub, cex = 0.8)
  xfit <- seq(min(var, na.rm = TRUE), max(var, na.rm = TRUE), length = 50)
  yfit <- dnorm(xfit, mean = mean(var, na.rm = TRUE), sd = sd(var, na.rm = TRUE))
  yfit <- yfit * diff(h$mids[1:2]) * length(var)
  lines(xfit, yfit, col = "blue", lwd = 2)
  mtext(txt, side = 3, outer = TRUE, line = -3)
}
