#' Clean the Reactome FI gene set table
#' 
#' The Cytoscape Reactome FI Geneset Analysis is cleaned before Enrichment Analysis.
#' 
#' The following cleaning procedures are made: 1) The "<" needs to be removed from the
#' FDR column. 2) Duplicate the GeneSet column. 3) Add a column of ones. 4) Replace
#' unicode characters with ASCII plain text.
#'
#' @param data data object
#' @param save if \code{TRUE}, the cleaned object is saved as a tab-delimited file.
#' @param file.path The file path to save the object.
#'
#' @return A cleaned gene set table.
#' @author Derek Chiu
#' @export
cleanGenes <- function(data, save = TRUE, file.path = "") {
  . <- FDR <- GeneSet <- GeneSet2 <- `P-value` <- Phenotype <- NULL
  res <- data %>% 
    mutate(FDR = as.numeric(gsub("<", "", FDR)),
           GeneSet2 = GeneSet,
           Phenotype = 1) %>% 
    mutate_each(funs(coding = iconv(., "latin1", "ASCII")),
                grep("^GeneSet", names(.))) %>% 
    select(GeneSet, GeneSet2, `P-value`, FDR, Phenotype) %>% 
    rename(GeneSet = GeneSet2, `1` = Phenotype)
  if (save)
    readr::write_delim(res, file.path, delim = "\t")
  return(res)
}