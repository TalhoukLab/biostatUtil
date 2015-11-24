## ----setup, echo=FALSE, message=FALSE------------------------------------
library(NanoString)
library(NanoStringNorm)
library(otta)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
data(rawOVCA2, rawPROT, rawOTTA, annot)

# Codeset 1, 2, 3 and annotations
cs1 <- rawOVCA2; cs2 <- rawPROT; cs3 <- rawOTTA; exp0 <- annot
exp0$geneRLF <- as.character(factor(exp0$geneRLF,
                     labels = c("HL1", "HL2", "HL3", "HuRef", "CS3", "mini", "CS1", "CS2")))

# Compute NanoString QC
exp.CS1 <- NanoStringQC(cs1, exp0[exp0$geneRLF == "CS1", ],
                        plots = FALSE, detect = 50, ttl = "CodeSet 1")
exp.CS2 <- NanoStringQC(cs2, exp0[exp0$geneRLF == "CS2", ],
                      plots = FALSE, sn = 100, ttl = "CodeSet 2")
exp.CS3 <- NanoStringQC(cs3, exp0[exp0$geneRLF == "CS3", ],
                        plots = TRUE, detect = 50, sn = 100, ttl = "CodeSet 3")

## ---- results='asis'-----------------------------------------------------
set.seed(12)
A <- matrix(rnorm(120), ncol = 10)
B <- matrix(rnorm(80), ncol = 10)
C <- matrix(rnorm(50), ncol = 10)
pander::pandoc.table(ratioMethod(A, B, C))

## ---- results='asis'-----------------------------------------------------
data(NanoString)
NanoString.mRNA[NanoString.mRNA$Name %in%
c('Eef1a1','Gapdh','Hprt1','Ppia','Sdha'), 'Code.Class'] <- 'Housekeeping'
out <- HKnorm(NanoString.mRNA)
pander::pandoc.table(head(out))

