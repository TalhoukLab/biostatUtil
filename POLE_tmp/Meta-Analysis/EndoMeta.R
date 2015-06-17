library(meta)

Name <- c("Portec", "Leuven", "TCGA", "Zurich")
n.trt <- c(15, 9, 18, 16)
n.ctrl <- c(94, 161, 211, 223)
col.trt <- c(4, 0, 0, 0)
col.ctrl <- c(46, 36, 21, 57)
endo <- data.frame(Name, n.trt, n.ctrl, col.trt, col.ctrl)
a <- meta.MH(n.trt, n.ctrl, col.trt, col.ctrl, data = endo, names = Name)
plot(a)
varianceFromCI(0.43,0.13,1.37,0.05)
varianceFromCI(0.18,0.01,3.11,0.05)
varianceFromCI(0.12,0.01,2.11,0.05)
varianceFromCI(0.21,0.01,4.26,0.05)

studlab <- c("Portec", "Leuven", "TCGA", "Billingsley")
effects <- log(c(0.43, 0.18, 0.12, 0.37))
se_effects <- c(sqrt(varianceFromCI(0.43,0.13,1.37,0.05)$var),
              sqrt(varianceFromCI(0.18,0.01,3.11,0.05)$var),
              sqrt(varianceFromCI(0.12,0.01,2.11,0.05)$var),
              sqrt(varianceFromCI(0.37,0.09,1.54,0.05)$var))

test <- metagen(effects, se_effects, studlab = studlab, sm = "HR", comb.fixed = T)
metagen(effects, se_effects, studlab = studlab, sm = "HR")
