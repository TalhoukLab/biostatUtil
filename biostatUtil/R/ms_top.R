#' Top variables in mass spectrometry analysis
#' 
#' Show top genes/peptides defined by significance level and absolute fold change.
#'
#' @param x data frame object returned by \code{ms_summarize}
#' @param alpha significance level
#' @param fc absolute fold change
#' @inheritParams ms_summarize
#'
#' @return A data frame showing the top variables.
#' @family Mass Spectrometry functions
#' @author Derek Chiu
#' @export
ms_top <- function(x, level = c("Gene", "Peptide"), alpha = 0.05, fc = 1.25,
                   path = NULL) {
  Var <- switch(match.arg(level),
                Gene = c("Gene", "Accession"),
                Peptide = "AGDSM")
  BHOP <- grep("(?=.*adj)(?=.*omnibus)(?=.*p-*val)", names(x),
               ignore.case = TRUE, perl = TRUE, value = TRUE)
  AFC <- grep("(?=.*abs)(?=.*effect)", names(x),
              ignore.case = TRUE, perl = TRUE, value = TRUE)
  topMat <- x %>% 
    extract(.[BHOP] < alpha & apply((.[AFC] > fc), 1, any), ) %>% 
    select(one_of(Var, BHOP, AFC))
  if (!is.null(path))
    readr::write_csv(topMat, path)
  return(topMat)
}