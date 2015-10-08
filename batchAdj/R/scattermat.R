#' Computes the between and within class scatter matrices
#' @param data data matrix
#' @param Y vector of classes
#' @return A list with the following elements
#' \item{B}{between class scatter matrix}
#' \item{W}{within class scatter matrix}
#' @author Aline Talhouk
#' @export
#' @examples
#' set.seed(4)
#' dat <- matrix(rnorm(100), ncol = 10)
#' batches <- sample(1:2, 10, replace = TRUE)
#' scattermat(dat, batches)
scattermat <- function(data, Y) {
  l <- ncol(data)
  clases <- unique(Y)
  nclass <- length(clases)
  B <- matrix(0, nrow = l, ncol = l)
  W <- matrix(0, nrow = l, ncol = l)
  overallmean <- apply(data, 2, mean)
  for (i in 1:nclass) {
    clasei <- which(Y == clases[i])
    xi <- data[clasei, ]
    mci <- apply(xi, 2, mean)  # MEAN PER CLASS
    x2i <- t(apply(xi, 1, function(x) x - mci))  # Xi-MeanXi
    W <- W + t(x2i) %*% x2i
    B <- B + length(clasei) * (mci - overallmean) %*% t(mci - overallmean)
  }
  return(list(B = B, W = W))
}
