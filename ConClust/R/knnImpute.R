#' Function to any missing clustering (due to resampling using K nearest neighbour)
#' @param res is a vector representing a column of the output from ConClust
#' @param tdat is the data matrix with samples are rows and genes as columns
#' @param seed set random seed for reproducibility
#' @return results matrix with missing values filled in except when fewer than 3 out 5 nearest neighbours are in agreement
#' @author Aline Talhouk
#' @export
knnImpute <- function(res, tdat = tdat, seed = 123456) {
  set.seed(seed)
  res.n <- res
  ind <- is.na(res)
  cl <- res[!ind]
  train <- tdat[!ind,]
  test <- tdat[ind,]
  knn.res <- class::knn(train, test, cl, k = 5, l = 3, prob = TRUE)
  res.n[ind] <- knn.res
  return(res.n)
}
