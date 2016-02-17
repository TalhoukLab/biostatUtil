pkgname <- "nanonstringr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "nanonstringr-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('nanonstringr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("HKnorm")
### * HKnorm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: HKnorm
### Title: Normalization to Housekeeping Genes (Single-Patient sample)
### Aliases: HKnorm

### ** Examples

library(NanoStringNorm)
data(NanoString)
NanoString.mRNA[NanoString.mRNA$Name %in%
c('Eef1a1','Gapdh','Hprt1','Ppia','Sdha'), 'Code.Class'] <- 'Housekeeping'

HKnorm(NanoString.mRNA)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("HKnorm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NanoStringQC")
### * NanoStringQC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NanoStringQC
### Title: QC for NanoString Data
### Aliases: NanoStringQC

### ** Examples

# Load otta package for raw datasets and annotation matrix
library(otta)
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



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NanoStringQC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("ratioMethod")
### * ratioMethod

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ratioMethod
### Title: Ratio method for batch adjustment
### Aliases: ratioMethod

### ** Examples

set.seed(12)
A <- matrix(rnorm(120), ncol = 10)
B <- matrix(rnorm(80), ncol = 10)
C <- matrix(rnorm(50), ncol = 10)
ratioMethod(A, B, C)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ratioMethod", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
