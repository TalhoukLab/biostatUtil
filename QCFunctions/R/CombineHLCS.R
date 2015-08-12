#' Combines HL Codeset 1 and Codeset 2 Nanostring Data
#'
#' This function matches the genes that are common to codeset 1 and codeset 2 (same accession number) and combines them after fixing nomenclature.
#' @param CS1 matrix of raw counts obtained from nCounter (rows are genes) with codeset 1. The first three columns must be labeled: c("Code.Class","Name","Accession") and contain that information.
#' @param CS2 matrix of raw counts obtained from nCounter (rows are genes) with codeset 2. The first three columns must be labeled: c("Code.Class","Name","Accession") and contain that information.
#' @param CS3 matrix of raw counts obtained from nCounter (rows are genes) with codeset 3. The first three columns must be labeled: c("Code.Class","Name","Accession") and contain that information. Defaults to Null for analysis of two codesets.
#' @return matrix of combined data with the first three columns labeled as c("Code.Class","Name","Accession")
#' @export
CombineHLCS<-function(CS1,CS2,CS3 = NULL){
  CS1.c <- CS1[CS1$Accession %in% CS2$Accession, ]
  CS2.c <- CS2[CS2$Accession %in% CS1$Accession, ]
  CS1.c <- CS1.c[order(CS1.c$Accession), ]
  CS2.c <- CS2.c[order(CS2.c$Accession), ]

  #Rename the genes
  CS1.c$Name[CS1.c$Name=="GJP2"] <- "GJB2"
  CS1.c$Name[CS1.c$Name=="LGAL1"] <- "LGALS1"
  CS1.c$Name[CS1.c$Name=="MORG1"] <- "WDR83"
  CS1.c$Name[CS1.c$Name=="TRA@"] <- "TRA"
  CS1.c$Name[CS1.c$Name=="TRAIL"] <- "TNFSF10"
  CS1.c$Name[CS1.c$Name=="BRDG1"] <- "STAP1"
  CS1.c$Name[CS1.c$Name=="CD56"] <- "NCAM1"
  CS1.c$Name[CS1.c$Name=="CD57"] <- "B3GAT1"
  CS1.c$Name[CS1.c$Name=="CD20"] <- "MS4A1"
  CS1.c$Name[CS1.c$Name=="CD26"] <- "DPP4"
  CS1.c$Name[CS1.c$Name=="CD30"] <- "TNFRSF8"
  CS1.c$Name[CS1.c$Name=="ABBC1"] <- "ABCC1"

  #Order the matrices by gene Name
  CS1.c <- CS1.c[order(CS1.c$Name), ]
  CS2.c <- CS2.c[order(CS2.c$Name), ]

if (!is.null(CS3)){
  CS3.c <- CS3[CS3$Accession %in% CS2.c$Accession, ]
  CS3.c <- CS3.c[order(CS3.c$Accession), ]
  CS3.c <- CS3.c[order(CS3.c$Name), ]
  if (all(CS2.c$Name == CS1.c$Name) & all(CS3.c$Name == CS1.c$Name)){
    CS3temp <- CS3.c[, -(1:3)]
    CS2temp <- CS2.c[, -(1:3)]
    CS1temp <- CS1.c[, -(1:3)]
    Combined <- cbind(CS1.c[, 1:3], CS1temp[, order(colnames(CS1temp))], CS2temp[, order(colnames(CS2temp))], CS3temp[, order(colnames(CS3temp))])
    rownames(Combined) <- Combined$Name
    return(Combined)
  }
} else if (all(CS2.c$Name == CS1.c$Name)) {
  CS2temp <- CS2.c[, -(1:3)]
  CS1temp <- CS1.c[, -(1:3)]
  Combined <- cbind(CS1.c[, 1:3], CS1temp[, order(colnames(CS1temp))], CS2temp[, order(colnames(CS2temp))])
  rownames(Combined) <- Combined$Name
  return(Combined)
}
else
{ print("Error: the gene names in the two sets don't match!") }
}
