## ----setup, echo=FALSE, message=FALSE------------------------------------
library(NanoString)
library(NanoStringNorm)
library(otta)
library(stringr)
library(dplyr)
library(ggplot2)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----codesets------------------------------------------------------------
data(rawOVCA2, rawPROT, rawOTTA, annot)

# Codeset 1, 2, 3 and annotations
cs1 <- rawOVCA2
cs2 <- rawPROT
cs3 <- rawOTTA
exp0 <- annot
exp0$geneRLF <- as.character(factor(exp0$geneRLF,
                     labels = c("HL1", "HL2", "HL3", "HuRef", "CS3", "mini", "CS1", "CS2")))

# Compute NanoString QC
exp.CS1 <- NanoStringQC(cs1, exp0[exp0$geneRLF == "CS1", ],
                        plots = FALSE, detect = 50, ttl = "CodeSet 1")
exp.CS2 <- NanoStringQC(cs2, exp0[exp0$geneRLF == "CS2", ],
                        plots = FALSE, sn = 100, ttl = "CodeSet 2")
exp.CS3 <- NanoStringQC(cs3, exp0[exp0$geneRLF == "CS3", ],
                        plots = TRUE, detect = 50, sn = 100, ttl = "CodeSet 3")

## ----pools---------------------------------------------------------------
pool1 <- exp0$File.Name[which(exp0$POOL1 == "Yes")]
pool2 <- exp0$File.Name[which(exp0$POOL2 == "Yes")]
pool3 <- exp0$File.Name[which(exp0$POOL3 == "Yes")]
cs3.names <- str_sub(names(cs3), start = 2)
cs2.names <- str_sub(names(cs2), start = 2)
cs3.pools <- cs3[, c(1:3, sort(match(c(pool1, pool2, pool3), cs3.names)))]
cs3.pool1 <- cs3[, sort(match(pool1, cs3.names))]
cs3.pool2 <- cs3[, sort(match(pool2, cs3.names))]
cs3.pool3 <- cs3[, sort(match(pool3, cs3.names))]
cs2.pools <- cs2[, c(1:3, sort(match(c(pool1, pool2, pool3), cs2.names)))]

# Compute NanoString QC
exp.CS3.pools <- NanoStringQC(cs3.pools, exp0[sort(match(c(pool1, pool2, pool3), cs3.names)) - 3, ],
                              plots = TRUE, detect = 50, sn = 100,
                              ttl = "CodeSet 3 in POOL 1-3")
exp.CS2.pools <- NanoStringQC(cs2.pools, exp0[sort(match(c(pool1, pool2, pool3), cs2.names)) - 3, ],
                              plots = TRUE, detect = 50, sn = 100,
                              ttl = "CodeSet 2 in POOL 1-3")

## ----pool_concord--------------------------------------------------------
cs3.pool1 <- cs3[, c(1:3, sort(match(pool1, cs3.names)))]
cs3.pool2 <- cs3[, c(1:3, sort(match(pool2, cs3.names)))]
cs3.pool3 <- cs3[, c(1:3, sort(match(pool3, cs3.names)))]

cs2.pool1 <- cs2[, c(1:3, sort(match(pool1, cs2.names)))]
cs2.pool2 <- cs2[, c(1:3, sort(match(pool2, cs2.names)))]
cs2.pool3 <- cs2[, c(1:3, sort(match(pool3, cs2.names)))]

cs3.pool1.c <- cs3.pool1[cs3.pool1$Name %in% intersect(cs3.pool1$Name, cs2.pool1$Name), ]
cs2.pool1.c <- cs2.pool1[cs2.pool1$Name %in% intersect(cs3.pool1$Name, cs2.pool1$Name), ]

cs3.pool2.c <- cs3.pool2[cs3.pool2$Name %in% intersect(cs3.pool2$Name, cs2.pool2$Name), ]
cs2.pool2.c <- cs2.pool2[cs2.pool2$Name %in% intersect(cs3.pool2$Name, cs2.pool2$Name), ]

cs3.pool3.c <- cs3.pool3[cs3.pool3$Name %in% intersect(cs3.pool3$Name, cs2.pool3$Name), ]
cs2.pool3.c <- cs2.pool3[cs2.pool3$Name %in% intersect(cs3.pool3$Name, cs2.pool3$Name), ]

pool1.dat <- merge(cs3.pool1.c, cs2.pool1.c, by = c("Name", "Code.Class")) %>% 
  tidyr::gather(CodeSets, Expr, which(startsWith(names(.), "X"))) %>% 
  mutate(CodeSets = ifelse(grepl("RNA", CodeSets), "CS2", "CS3")) %>% 
  arrange(Name) %>% 
  select(Name, CodeSets, Expr)

pool2.dat <- merge(cs3.pool2.c, cs2.pool2.c, by = c("Name", "Code.Class")) %>% 
  tidyr::gather(CodeSets, Expr, which(startsWith(names(.), "X"))) %>% 
  mutate(CodeSets = ifelse(grepl("RNA", CodeSets), "CS2", "CS3")) %>% 
  arrange(Name) %>% 
  select(Name, CodeSets, Expr)

pool3.dat <- merge(cs3.pool3.c, cs2.pool3.c, by = c("Name", "Code.Class")) %>% 
  tidyr::gather(CodeSets, Expr, which(startsWith(names(.), "X"))) %>% 
  mutate(CodeSets = ifelse(grepl("RNA", CodeSets), "CS2", "CS3")) %>% 
  arrange(Name) %>% 
  select(Name, CodeSets, Expr)

# Randomly select 20 genes from each pool
set.seed(2016)
ngenes <- 20
pool1.rand <- pool1.dat %>% 
  magrittr::extract(.$Name %in% sample(unique(.$Name), ngenes), )
pool2.rand <- pool2.dat %>% 
  magrittr::extract(.$Name %in% sample(unique(.$Name), ngenes), )
pool3.rand <- pool3.dat %>% 
  magrittr::extract(.$Name %in% sample(unique(.$Name), ngenes), )

# Create side-by-side boxplots
ggplot(pool1.rand, aes(x = CodeSets, y = Expr)) +
  geom_boxplot() +
  facet_wrap(~ Name)

ggplot(pool2.rand, aes(x = CodeSets, y = Expr)) +
  geom_boxplot() +
  facet_wrap(~ Name)

ggplot(pool3.rand, aes(x = CodeSets, y = Expr)) +
  geom_boxplot() +
  facet_wrap(~ Name)

## ----ratioMethod, results='asis'-----------------------------------------
set.seed(12)
A <- matrix(rnorm(120), ncol = 10)
B <- matrix(rnorm(80), ncol = 10)
C <- matrix(rnorm(50), ncol = 10)
pander::pandoc.table(ratioMethod(A, B, C))

## ----HKnorm, results='asis'----------------------------------------------
data(NanoString)
NanoString.mRNA[NanoString.mRNA$Name %in%
c('Eef1a1','Gapdh','Hprt1','Ppia','Sdha'), 'Code.Class'] <- 'Housekeeping'
out <- HKnorm(NanoString.mRNA)
pander::pandoc.table(head(out))

