#' Function that computes the scatter matrix
#' @param B is the between class scatter matrix
#' @param W is the withing class scatter matrix
#' @param Y is a vector of classes
#' @author Aline Talhouk
scattermat <- function(data, Y) {
  l <- dim(data)[2]        #CALCULATE SIZE OF DATA
  clases <- unique(Y)       #GET VECTOR OF CLASSES
  tot_clases <- length(clases) #HOW MANY CLASSES
  B <- matrix(0, nrow = l, ncol = l)   #INIT B AND W
  W <- matrix(0, nrow = l, ncol = l)         
  overallmean <- apply(data, 2, mean)    #MEAN OVER ALL DATA
  for (i in 1:tot_clases) {
    clasei <- which(Y == clases[i]) #GET DATA FOR EACH CLASS
    xi <- data[clasei, ]
    mci <- apply(xi, 2, mean)                       #MEAN PER CLASS
    x2i <- t(apply(xi, 1, function(x) { x - mci}))  #Xi-MeanXi
    W <- W + t(x2i) %*% x2i                         #CALCULATE W
    B <- B + length(clasei) * (mci-overallmean) %*% t(mci - overallmean) #CALCULATE B
  }
  return(list(B = B, W = W))
}

