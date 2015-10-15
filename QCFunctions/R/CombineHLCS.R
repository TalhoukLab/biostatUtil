#' Combines HL Codeset 1 and Codeset 2 Nanostring Data
#'
#' Matches genes that are common to Codeset 1 and Codeset 2 (same accession number) and combines them after fixing nomenclature.
#' Supports functionality for combining a third codeset.
#'
#' If \code{CS3} is not {NULL}, then \code{CombineHLCS} combines three codesets, given that the gene names match. The first three columns
#' of \code{CS1}, \code{CS2}, and \code{CS3} must be labeled \code{c("Code.Class", "Name", "Accession")} and contain that information.
#'
#' @param CS1 matrix of raw counts obtained from nCounter (rows are genes) with codeset 1.
#' @param CS2 matrix of raw counts obtained from nCounter (rows are genes) with codeset 2.
#' @param CS3 matrix of raw counts obtained from nCounter (rows are genes) with codeset 3.
#' Defaults to \code{NULL} for analysis of two codesets.
#' @return A matrix of combined data with the first three columns labeled as c("Code.Class", "Name", "Accession")
#' @author Aline Talhouk
#' @import dplyr
#' @export
#' @examples
#' library(otta)
#' data(rawDUP3, rawDUP4, rawDUP5)
#' ds1 <- rawDUP3; ds2 <- rawDUP4; ds3 <- rawDUP5
#' CombineHLCS(ds1[, 1:3], ds2[, 1:3], ds3[, 1:3])
CombineHLCS <- function(CS1, CS2, CS3 = NULL) {
  assertthat::assert_that(check_colnames(CS1, CS2))
  . <- Name <- Accession <- NULL
  CS1.c <- CS1 %>%
    mutate(Name = stringr::str_replace_all(
      Name, c("GJP2" = "GJB2", "LGAL1" = "LGALS1", "MORG1" = "WDR83",
              "TRA@" = "TRA", "TRAIL" = "TNFSF10", "BRDG1" = "STAP1",
              "CD56" = "NCAM1", "CD57" = "B3GAT1", "CD20" = "MS4A1",
              "CD26" = "DPP4", "CD30^" = "TNFRSF8", "ABBC1" = "ABCC1"))) %>%
    filter(Accession %in% CS2$Accession) %>%
    arrange(order(Accession)) %>%
    magrittr::extract(order(.$Name), )
  CS2.c <- CS2 %>%
    filter(Accession %in% CS1$Accession) %>%
    arrange(order(Accession)) %>%
    magrittr::extract(order(.$Name), )
  if (!is.null(CS3)) {
    assertthat::assert_that(check_colnames(CS3))
    CS3.c <- CS3 %>%
      filter(Accession %in% CS2.c$Accession) %>%
      arrange(order(Accession)) %>%
      magrittr::extract(order(.$Name), )
    if (all(CS2.c$Name == CS1.c$Name & CS3.c$Name == CS1.c$Name)) {
      Combined <- CS1.c[, 1:3] %>%
        cbind(CS1.c[, -(1:3)][, order(colnames(CS1.c[, -(1:3)]))],
              CS2.c[, -(1:3)][, order(colnames(CS2.c[, -(1:3)]))],
              CS3.c[, -(1:3)][, order(colnames(CS3.c[, -(1:3)]))]) %>%
        magrittr::set_rownames(.$Name)
      return(Combined)
    } else {
      stop("The gene names in at least two codesets don't match.")
    }
  } else if (all(CS2.c$Name == CS1.c$Name)) {
    Combined <- CS1.c[, 1:3] %>%
      cbind(CS1.c[, -(1:3)][, order(colnames(CS1.c[, -(1:3)]))],
            CS2.c[, -(1:3)][, order(colnames(CS2.c[, -(1:3)]))]) %>%
      magrittr::set_rownames(.$Name)
    return(Combined)
  } else {
    stop("The gene names in the two codesets don't match.")
  }
}
