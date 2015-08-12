#' Plotting function for Reliability
#' @param method1 is the measurements obtained in batch 1 or using method 1
#' @param method2 is the measurements obtained in batch 2 or using method 2
#' @param Ptype is the type of plot to be outputted c("scatter","MAplot")
#' @param metrics if return metrics is set to true(default is FALSE) returns Rc, Ca and R2
#' @param xlabel is the label to be used for x axis
#' @param ylabel is the label to be used for y axis
#' @param title the title for the main plot
#' @param subtitle is the subtitle if requested
#' @param xrange range of x axis
#' @param yrange range of y axis
#' @param MArange MA range
#' @export


CCplot <- function(method1, method2, Ptype = "none", metrics = FALSE,
                   xlabel = "", ylabel = "", title = "", subtitle = "",
                   xrange = NULL, yrange = NULL, MArange = c(-3.5, 5.5)){
  ## Concordance correlation plot
  tmp.ccc <- epiR::epi.ccc(method1, method2, ci = "z-transform",
                     conf.level = 0.95)
  cclab <- paste("Rc: ", round(tmp.ccc$rho.c[,1], digits = 2), " (",
                 round(tmp.ccc$rho.c[,2], digits = 2), " - ",
                 round(tmp.ccc$rho.c[,3], digits = 2), ")", sep = "")
  r2lab <- bquote(R^2 : .( round(cor(method1,method2),2)))
  Acc <- paste("Ca:", round(tmp.ccc$C.b, 2))
  loc <- paste("Location shift:", round(tmp.ccc$l.shift, 2), sep = "")
  scl <- paste("Scale shift:", round(tmp.ccc$s.shift, 2), sep = "")
  z <- lm(method2 ~ method1)
  tmp.mean <- mean(tmp.ccc$blalt$delta)
  tmp.sd <- sqrt(var(tmp.ccc$blalt$delta))
if (is.null(xrange)) {
  xrange <- range(method1)
}

if (is.null(yrange)) {
    yrange <- range(method2)
}

  if (Ptype == "scatter") { # Scatter Plot
    plot(method1, method2, xlab = xlabel, xlim = xrange, ylim = yrange,
         ylab = ylabel, pch = 16, sub = paste("(",subtitle,")"))
    abline(a = 0, b = 1, lty = 2)
    abline(z, lty = 1)
    usr <- par("usr")  	# get user coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(0.5, 0.18, r2lab, adj = 0)
    text(0.5, 0.12, Acc, adj = 0)
    text(0.5, 0.05, cclab, adj = 0)
    par(usr = usr)	# restore original user coordinates
  } else if (Ptype=="MAplot") { #Bland-Altman or MAplot
    plot(tmp.ccc$blalt$mean, tmp.ccc$blalt$delta, pch = 16, xlab = "Average",
         ylab = "Difference", sub=paste("(",subtitle,")"), ylim = MArange )
    abline(h = tmp.mean, lty = 1, col = "gray")
    abline(h = tmp.mean - (2 * tmp.sd), lty = 2, col = "gray")
    abline(h = tmp.mean + (2 * tmp.sd), lty = 2, col = "gray")
    abline(h = 0, lty = 1, col = "red")
    usr <- par("usr")  	# get user coordinates
    par(usr = c(0, 1, 0, 1)) # new relative user coordinates
    text(0.5, 0.95, r2lab, adj = 0)
    text(0.5, 0.88, Acc, adj = 0)
    text(0.5, 0.81, cclab, adj = 0)
    par(usr = usr)	# restore original user coordinates
  }
  if (metrics==TRUE){
    return(c(Rc = round(tmp.ccc$rho.c[,1], digits = 2), Ca = round(tmp.ccc$C.b, 2), R2 = round(cor(method1,method2),2)))
  }
}
