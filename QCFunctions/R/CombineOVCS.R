#' Combines Ovarian Codeset 1 and Codeset 2 Nanostring Data
#'
#' Matches genes that are common to Codeset 1 and Codeset 2 (same accession number) and combines them after fixing nomenclature.
#'
#' The four genes "DUSP4", "IGKC", "LDHA", and "PTHLH" have non-matching accession and are added to both codesets. The first three columns
#' of \code{CS1}, and \code{CS2} must be labeled \code{c("Code.Class", "Name", "Accession")} and contain that information.
#'
#' @param CS1 matrix of raw counts obtained from nCounter (rows are genes) with codeset 1.
#' @param CS2 matrix of raw counts obtained from nCounter (rows are genes) with codeset 2.
#' @return A matrix of combined data with the first three columns labeled as c("Code.Class", "Name", "Accession")
#' @author Aline Talhouk, Derek Chiu
#' @import dplyr
#' @export
#' @examples
#' library(otta)
#' data(rawOVCA2, rawPROT)
#' cs1 <- rawOVCA2; cs2 <- rawPROT
#' CombineOVCS(cs1[, 1:3], cs2[, 1:3])
CombineOVCS <- function(CS1, CS2) {
  assertthat::assert_that(check_colnames(CS1, CS2))
  . <- Name <- Accession <- NULL
  AG <- c("DUSP4", "IGKC", "LDHA", "PTHLH")
  CS1.c <- CS1 %>%
    filter(Accession %in% CS2$Accession) %>%
    arrange(order(Accession)) %>%
    mutate(Name = stringr::str_replace_all(
      Name, c("SOX11" = "LOX", "LDHC_V2" = "LDHC"))) %>%
    rbind(., CS1[CS1$Name %in% AG, ]) %>%
    magrittr::extract(order(.$Name), )
  CS2.c <- CS2 %>%
    filter(Accession %in% CS1$Accession & Name != "ERBB3") %>%
    arrange(order(Accession)) %>%
    rbind(., CS2[CS2$Name %in% AG, ]) %>%
    magrittr::extract(order(.$Name), )
  if (all(CS1.c$Name == CS2.c$Name)) {
    Combined <- CS1.c[, 1:3] %>%
      cbind(CS1.c[, -(1:3)][, order(colnames(CS1.c[, -(1:3)]))],
            CS2.c[, -(1:3)][, order(colnames(CS2.c[, -(1:3)]))]) %>%
      magrittr::set_rownames(.$Name)
    return(Combined)
  } else {
    stop("The gene names in the two sets don't match.")
  }
}
