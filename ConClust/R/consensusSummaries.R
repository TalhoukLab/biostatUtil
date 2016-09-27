#' Compute consensus matrices and classes from ConClust output
#'
#' Given an output from ConClust, returns a list of consensus matrices and classes
#' for each algorithm.
#'
#' @param res output result from ConClust
#' @param k desired number of clusters
#' @param dir directory to save output
#' @param fileName file name of the written object
#' @return A list with summaries for each algorithm. Each algorithm has a list with
#' two elements: consensus_matrix and consensusClass.
#' @author Derek Chiu
#' @export
consensusSummaries <- function(res, k, dir = NULL, fileName = "results_CC") {
  con.mats <- plyr::alply(res, 3, consensus_matrix,
                          .progress = "text", .dims = TRUE)
  con.cls <- plyr::llply(con.mats, consensusClass, k = k)

  z <- list(consensus_matrix = con.mats, consensusClass = con.cls)
  zv <- unlist(unname(z), recursive = FALSE)
  out <- split(setNames(zv, rep(names(z), lengths(z))), names(zv))
  if (!is.null(dir))
    saveRDS(out, paste0(dir, fileName, ".rds"), compress = "xz")
  return(out)
}
