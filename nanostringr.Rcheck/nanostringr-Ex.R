pkgname <- "nanostringr"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "nanostringr-Ex.timings", pos = 'CheckExEnv')
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
library('nanostringr')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("CCplot")
### * CCplot

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: CCplot
### Title: Concordance Correlation Plot
### Aliases: CCplot

### ** Examples

# Simulate normally distributed data
set.seed(12)
a1 <- rnorm(20) + 2
a2 <- a1 + rnorm(20, 0, 0.15)
a3 <- a1 + rnorm(20, 0, 0.15) + 1.4
a4 <- 1.5 * a1 + rnorm(20, 0, 0.15)
a5 <- 1.3 * a1 + rnorm(20, 0, 0.15) + 1
a6 <- a1 + rnorm(20, 0, 0.8)
par(mfrow = c(3, 2), mar = c(5.1, 4.1, 1.5, 1.5))

# Scatterplots
CCplot(a1, a1, Ptype = "scatter", "X", "Y", "Perfect Agreement", subtitle = letters[1])
CCplot(a1, a2, Ptype = "scatter", "X", "Y", "Very Good Agreement", subtitle = letters[2])
CCplot(a1, a3, Ptype = "scatter", "X", "Y", "Location Shift", subtitle = letters[3])
CCplot(a1, a4, Ptype = "scatter", "X", "Y", "Scale Shift", subtitle = letters[4])
CCplot(a1, a5, Ptype = "scatter", "X", "Y", "Location and Scale Shift", subtitle = letters[5])
CCplot(a1, a6, Ptype = "scatter", "X", "Y", "Measurement Error", subtitle = letters[6])

# MAplots
CCplot(a1, a1, Ptype = "MAplot", "X", "Y", "Perfect Agreement", subtitle = letters[1])
CCplot(a1, a2, Ptype = "MAplot", "X", "Y", "Very Good Agreement", subtitle = letters[2])
CCplot(a1, a3, Ptype = "MAplot", "X", "Y", "Location Shift", subtitle = letters[3])
CCplot(a1, a4, Ptype = "MAplot", "X", "Y", "Scale Shift", subtitle = letters[4])
CCplot(a1, a5, Ptype = "MAplot", "X", "Y", "Location and Scale Shift", subtitle = letters[5])
CCplot(a1, a6, Ptype = "MAplot", "X", "Y", "Measurement Error", subtitle = letters[6])



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("CCplot", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("HKnorm")
### * HKnorm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: HKnorm
### Title: Normalization to Housekeeping Genes
### Aliases: HKnorm

### ** Examples

HKnorm(ovd.r)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("HKnorm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("NanoStringQC")
### * NanoStringQC

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: NanoStringQC
### Title: QC metrics for NanoString Data
### Aliases: NanoStringQC

### ** Examples

exp.OVD <-subset(expQC,OVD=="Yes")
expOVD <- NanoStringQC(ovd.r,exp.OVD)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("NanoStringQC", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("refMethod")
### * refMethod

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: refMethod
### Title: Reference-based approach for batch adjustment
### Aliases: refMethod

### ** Examples

set.seed(12)
A <- matrix(rnorm(120), ncol = 10)
B <- matrix(rnorm(80), ncol = 10)
C <- matrix(rnorm(50), ncol = 10)
refMethod(A, B, C)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("refMethod", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
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
