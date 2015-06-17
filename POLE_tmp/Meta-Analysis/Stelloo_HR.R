# Read in raw data
dat.raw <- read.csv("POLE_tmp/data/ClinicalandpathinfoofPOLEmutatedandWTEC3_fromCH_Jun82015.csv")

# Munge data to proper format
library(stringr)
library(plyr)
library(dplyr)
library(Kmisc)
library(survival)

dat <- dat.raw %>%
  mutate(POLE.mutation = factor(str_sub(POLE.mutation, start = 1, end = 3)),
         FIGO.2009 = gsub(",", "", 
                          apply(str_split_fixed(FIGO.2009, " ", 3)[, -3],
                                1, function(x) paste(x, collapse = " "))),
         FIGO.short = str_sub(FIGO.2009, start = 7, end = -2) %>%
           gsub("C", "", .) %>%
           gsub("1", "I", .) %>%
           gsub("3", "III", .) %>%
           factor) %>%
  mutate_each_(funs(swap(., from = c(0, 1), to = c(1, 0))),
               names(.)[grep("Censor", names(.))]) %>%
  mutate_each_(funs(as.factor), names(.)[grep("Chemo|Radia", names(.))]) %>%
  filter(Residual_Disease != "Yes") %>%
  droplevels

# Compute multivariable model POLE mutation HRs for OS, DSS, RFS
# TO DO: need Firth's Correction for DSS and RFS
mod.os <- coxph(Surv(Follow.up.duration, Censor.OS) ~ POLE.mutation + Age.at.SX + Chemotherapy + Radiation, dat)
# mod.dss <- coxph(Surv(Follow.up.duration, Censor.DSS) ~ POLE.mutation + Age.at.SX + Chemotherapy + Radiation, dat)
# mod.rfs <- coxph(Surv(Follow.up.duration, Censor.recurrence) ~ POLE.mutation + Age.at.SX + Chemotherapy + Radiation, dat)

HR.os <- exp(coef(mod.os))[grep("POLE", names(coef(mod.os)))]
# HR.dss <- exp(coef(mod.dss))[grep("POLE", names(coef(mod.dss)))]
# HR.rfs <- exp(coef(mod.rfs))[grep("POLE", names(coef(mod.rfs)))]

se.os <- summary(mod.os)$coefficients["POLE.mutationwt", "se(coef)"]
# se.dss <- summary(mod.dss)$coefficients["POLE.mutationwt", "se(coef)"]
# se.rfs <- summary(mod.rfs)$coefficients["POLE.mutationwt", "se(coef)"]


