# Load packages and read in raw data
library(stringr)
library(plyr)
library(dplyr)
library(Kmisc)
library(biostatUtil)
dat.raw <- read.csv("./POLE_tmp/data/ClinicalandpathinfoofPOLEmutatedandWTEC3_fromCH_Jun82015.csv")

# Munge data to proper format
dat.overall <- dat.raw %>%
  mutate(POLE.mutation = factor(str_sub(POLE.mutation, start = 1, end = 3)),
         FIGO.2009 = gsub(",", "", 
                          apply(str_split_fixed(FIGO.2009, " ", 3)[, -3],
                                1, function(x) paste(x, collapse = " "))),
         FIGO.short = str_sub(FIGO.2009, start = 7, end = -2) %>%
           gsub("C", "", .) %>%
           gsub("1", "I", .) %>%
           gsub("3", "III", .) %>%
           factor,
         Stage = as.factor(ifelse(FIGO.short == "I", 0, 1))) %>%
  mutate_each_(funs(swap(., from = c(0, 1), to = c(1, 0))),
               names(.)[grep("Censor", names(.))]) %>%
  mutate_each_(funs(as.factor), names(.)[grep("Chemo|Radia", names(.))]) %>% 
  mutate(Chemotherapy = as.factor(ifelse(is.na(Chemotherapy), "NA", Chemotherapy)),
         Radiation = as.factor(ifelse(is.na(Radiation), "NA", Radiation)))

# Remove cases with residual disease for DSS and RFS outcomes
dat.residual <- dat.overall %>%
  filter(Residual_Disease != "Yes") %>%
  droplevels

# univariable POLE mutation HRs for OS, DSS, RFS
uni.os <- prettyCoxph(Surv(Follow.up.duration, Censor.OS) ~ POLE.mutation,
                      dat.overall, use.firth = -1)
uni.dss <- prettyCoxph(Surv(Follow.up.duration, Censor.DSS) ~ POLE.mutation,
                         dat.residual, use.firth = -1)
uni.rfs <- prettyCoxph(Surv(Follow.up.duration, Censor.recurrence) ~ POLE.mutation,
                         dat.residual, use.firth = -1)

# multivariable POLE mutation HRs for OS, DSS, RFS
multi.os <- prettyCoxph(Surv(Follow.up.duration, Censor.OS) ~ POLE.mutation +
                        Age.at.SX + Chemotherapy + Radiation + Stage,
                      dat.overall, use.firth = -1)
multi.dss <- prettyCoxph(Surv(Follow.up.duration, Censor.DSS) ~ POLE.mutation +
                        Age.at.SX + Chemotherapy + Radiation + Stage,
                      dat.residual, use.firth = -1)
multi.rfs <- prettyCoxph(Surv(Follow.up.duration, Censor.recurrence) ~ POLE.mutation +
                        Age.at.SX + Chemotherapy + Radiation + Stage,
                      dat.residual, use.firth = -1)
