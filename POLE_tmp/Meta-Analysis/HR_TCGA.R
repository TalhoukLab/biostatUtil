# Load packages and read in raw data
library(dplyr)
library(tcgaEndometrial)
library(biostatUtil)
data("tcgaEd")

# Munge data to proper format
dat <- tcgaEd %>% 
  select(AGE, OS_MONTHS, OS_STATUS, GRADE, 
         TUMOR_STAGE_2009, HISTOLOGICAL_SUBTYPE, POLE.mut.value.label) %>%
  rename(STAGE = TUMOR_STAGE_2009, POLE = POLE.mut.value.label) %>%
  mutate(OS_STATUS = ifelse(OS_STATUS == "os.event", 1, 0),
         GRADE = as.factor(ifelse(GRADE == "Grade 3", 1, 0)),
         STAGE = as.factor(ifelse(STAGE == "Stage I", 0, 1)),
         HISTOLOGICAL_SUBTYPE = as.factor(ifelse(HISTOLOGICAL_SUBTYPE == "Endometrioid", 0, 1)),
         POLE = relevel(POLE, "wild type"))

# Multivariable POLE mutation HRs for OS
multi.os <- prettyCoxph(Surv(OS_MONTHS, OS_STATUS) ~ POLE +
                          AGE + GRADE + STAGE + HISTOLOGICAL_SUBTYPE,
                        dat, use.firth = -1)
multi.os$output
