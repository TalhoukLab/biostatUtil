context("NanoStringQC")

library(otta)
data(rawOVCA2, rawPROT, rawOTTA, annot)
cs1 <- rawOVCA2
cs2 <- rawPROT
cs3 <- rawOTTA
exp0 <- annot
exp0$geneRLF <- as.character(factor(exp0$geneRLF,
                     labels = c("HL1", "HL2", "HL3", "HuRef", "CS3", "mini", "CS1", "CS2")))
