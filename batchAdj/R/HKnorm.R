#' Normalization to Housekeeping Genes (Single-Patient sample)
#'
#' This functions normalizes the gene expression of NanoString nCounter data to housekeeping genes. This is done by subtracting the average log housekeeping gene expression.
#' @param raw.data matrix of raw counts obtained from nCounter (rows are genes). The first three columns must be labeled: c("Code.Class","Name","Accession") and contain that information.
#' @return matrix of log normalized data in the same format but without reference genes.


HKnorm <- function(raw.data) {
  rawdat <- raw.data[, -(1:3)] + 0.0001
  rownames(rawdat) <- raw.data$Name
  hks <- raw.data$Code.Class == "Housekeeping"
  refs <- raw.data$Code.Class != "Endogenous"
  HG <- rawdat[hks, ]
  raw <- rawdat[!refs, ]
  
  logXpr <- log(raw, base = 2)
  logHK <- apply(log(HG, base = 2), 2, mean)
  norm <- t(apply(logXpr, 1, function(x) x - logHK))
  normdat <- cbind(raw.data[!refs,1:3], norm) 
  rownames(normdat) <- raw.data$Name[!refs]
  return(normdat)
}
