library(biostatUtil)
library(meta)

# Recurrence-Free Survival Hazard Ratios
studlab <- c("PORTEC", "Leuven", "TCGA", "Billingsley", "Meng")
effects <- log(c(0.43, 0.18, 0.12, 0.37, 0.7558702))
se_effects <- c(sdFromCI(0.43, 0.13, 1.37)$sd,
                sdFromCI(0.18, 0.01, 3.11)$sd,
                sdFromCI(0.12, 0.01, 2.11)$sd,
                sdFromCI(0.37, 0.09, 1.54)$sd,
                sdFromCI(0.7558702, 0.03634426, 112.860470)$sd)
metagen(effects, se_effects, studlab, sm = "HR", comb.fixed = T)

# Cancer-Specific Survival Hazard Ratios
studlab <- c("PORTEC", "Leuven", "Zurich/Basel", "Meng")
effects <- log(c(0.19, 0.66, 0.21, 0.8880270))
se_effects <- c(sdFromCI(0.19, 0.03, 1.44)$sd,
                sdFromCI(0.66, 0.04, 11.4)$sd,
                sdFromCI(0.21, 0.01, 4.26)$sd,
                sdFromCI(0.8880270, 0.04420431, 131.750737)$sd)
metagen(effects, se_effects, studlab, sm = "HR", comb.fixed = T)

# Five-year Survival Rates
studlab <- c("PORTEC", "Stelloo", "Billingsley", "TCGA", "Meng")
effects <- c(0.9725, 0.93, 0.9622, 1, 1)
se_effects <- c(sqrt(0.9725 * (1 - 0.9725) / 48),
                sqrt(0.93 * (1 - 0.93) / 14),
                sqrt(0.9622 * (1 - 0.9622) / 30),
                sqrt(1 / 17),
                sqrt(1 / 8))
metagen(effects, se_effects, studlab, sm = "PRAW", comb.fixed = T)
