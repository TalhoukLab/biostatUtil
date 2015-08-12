#' Combines Ovarian Codeset 1 and Codeset 2 Nanostring Data
#'
#' This function matches the genes that are common to codeset 1 and codeset 2 (same accession number) and combines them after fixing nomenclature.
#' @param CS1 matrix of raw counts obtained from nCounter (rows are genes) with codeset 1. The first three columns must be labeled: c("Code.Class","Name","Accession") and contain that information.
#' @param CS2 matrix of raw counts obtained from nCounter (rows are genes) with codeset 2. The first three columns must be labeled: c("Code.Class","Name","Accession") and contain that information.
#' @return matrix of combined data with the first three columns labeled as c("Code.Class","Name","Accession")
#' @export
CombineOVCS <- function(CS1, CS2) {
  CS1.c <- CS1[CS1$Accession %in% CS2$Accession, ]
  CS2.c <- CS2[CS2$Accession %in% CS1$Accession, ]
  CS1.c <- CS1.c[order(CS1.c$Accession), ]
  CS2.c <- CS2.c[order(CS2.c$Accession), ]

  #remove ERBB3 from codeset 2
  CS2.c <- CS2.c[!CS2.c$Name=="ERBB3", ]

  #rename SOX11 and LDHC in codeset1
  CS1.c$Name[CS1.c$Name == "SOX11"] <- "LOX"
  CS1.c$Name[CS1.c$Name == "LDHC_V2"] <- "LDHC"

  #Add the additional four genes with non-matching accession
  AG <- c("DUSP4", "IGKC", "LDHA", "PTHLH")
  CS1.c <- rbind(CS1.c, CS1[CS1$Name %in% AG, ])
  CS2.c <- rbind(CS2.c, CS2[CS2$Name %in% AG, ])

  #Order the matrices by gene Name
  CS1.c <- CS1.c[order(CS1.c$Name), ]
  CS2.c <- CS2.c[order(CS2.c$Name), ]

  if (all(CS1.c$Name == CS2.c$Name)){
    CS2temp <- CS2.c[, -(1:3)]
    CS1temp <- CS1.c[, -(1:3)]
    Combined <- cbind(CS1.c[, 1:3], CS1temp[, order(colnames(CS1temp))], CS2temp[, order(colnames(CS2temp))])
    rownames(Combined) <- Combined$Name
    return(Combined)
  }
  else
  { print("Error: the gene names in the two sets don't match!") }
}
