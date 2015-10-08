#' Combines HL Codeset 1 and Codeset 2 Nanostring Data
#'
#' Matches genes that are common to Codeset 1 and Codeset 2 (same accession number) and combines them after fixing nomenclature.
#' Supports functionality for combining a third codeset.
#' @param CS1 matrix of raw counts obtained from nCounter (rows are genes) with codeset 1. The first three columns must be labeled: c("Code.Class", "Name", "Accession") and contain that information.
#' @param CS2 matrix of raw counts obtained from nCounter (rows are genes) with codeset 2. The first three columns must be labeled: c("Code.Class", "Name", "Accession") and contain that information.
#' @param CS3 matrix of raw counts obtained from nCounter (rows are genes) with codeset 3. The first three columns must be labeled: c("Code.Class", "Name", "Accession") and contain that information. Defaults to \code{NULL} for analysis of two codesets.
#' @return A matrix of combined data with the first three columns labeled as c("Code.Class", "Name", "Accession")
#' @author Aline Talhouk
#' @export
#' @examples
#' library(otta)
#' data(rawDUP3, rawDUP4, rawDUP5)
#' ds1 <- rawDUP3; ds2 <- rawDUP4; ds3 <- rawDUP5
#' CombineHLCS(ds1[, 1:3], ds2[, 1:3], ds3[, 1:3])
CombineHLCS <- function(CS1, CS2, CS3 = NULL) {
  CS1.c <- CS1[CS1$Accession %in% CS2$Accession, ]
  CS2.c <- CS2[CS2$Accession %in% CS1$Accession, ]
  CS1.c <- CS1.c[order(CS1.c$Accession), ]
  CS2.c <- CS2.c[order(CS2.c$Accession), ]

  CS1.c$Name <- stringr::str_replace_all(
    CS1.c$Name, c("GJP2" = "GJB2", "LGAL1" = "LGALS1", "MORG1" = "WDR83",
                  "TRA@" = "TRA", "TRAIL" = "TNFSF10", "BRDG1" = "STAP1",
                  "CD56" = "NCAM1", "CD57" = "B3GAT1", "CD20" = "MS4A1",
                  "CD26" = "DPP4", "CD30^" = "TNFRSF8", "ABBC1" = "ABCC1"))

  # Order the matrices by gene Name
  CS1.c <- CS1.c[order(CS1.c$Name), ]
  CS2.c <- CS2.c[order(CS2.c$Name), ]

  if (!is.null(CS3)) {
    CS3.c <- CS3[CS3$Accession %in% CS2.c$Accession, ]
    CS3.c <- CS3.c[order(CS3.c$Accession), ]
    CS3.c <- CS3.c[order(CS3.c$Name), ]
    if (all(CS2.c$Name == CS1.c$Name) & all(CS3.c$Name == CS1.c$Name)) {
      CS3temp <- CS3.c[, -(1:3)]
      CS2temp <- CS2.c[, -(1:3)]
      CS1temp <- CS1.c[, -(1:3)]
      Combined <- cbind(CS1.c[, 1:3],
                        CS1temp[, order(colnames(CS1temp))],
                        CS2temp[, order(colnames(CS2temp))],
                        CS3temp[, order(colnames(CS3temp))])
      rownames(Combined) <- Combined$Name
      return(Combined)
    } else {
      stop("The gene names in at least two codesets don't match!")
    }
  } else if (all(CS2.c$Name == CS1.c$Name)) {
    CS2temp <- CS2.c[, -(1:3)]
    CS1temp <- CS1.c[, -(1:3)]
    Combined <- cbind(CS1.c[, 1:3],
                      CS1temp[, order(colnames(CS1temp))],
                      CS2temp[, order(colnames(CS2temp))])
    rownames(Combined) <- Combined$Name
    return(Combined)
  } else {
    stop("The gene names in the two codesets don't match!")
  }
}
