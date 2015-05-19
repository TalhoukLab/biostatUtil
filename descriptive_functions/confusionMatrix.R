confusionMatrix <- function(x, y, seed = 20, num.boot = 1000,
                            conf.level = 0.95, digits = 4) {
  require(Hmisc)
  cat("Confusion Matrix", "\n")
  CM = table(Prediction = x, Reference = y)
  print(CM)
  
  # Compute marginal totals and correct predictions
  m1 = CM[1, 1] + CM[2, 1]
  m2 = CM[1, 2] + CM[2, 2]
  n1 = CM[1, 1] + CM[1, 2]
  n2 = CM[2, 1] + CM[2, 2]
  PO = CM[1, 1] + CM[2, 2]
  
  # Calculate statistics and obtain confidence intervals
  Accuracy = round(binconf(PO, m1 + m2, alpha = 1 - conf.level,
                           method = "wilson"), digits)
  Sensitivity = round(binconf(CM[1, 1], m1, alpha = 1 - conf.level,
                              method = "wilson"), digits)
  Specificity = round(binconf(CM[2, 2], m2, alpha = 1 - conf.level,
                              method = "wilson"), digits)
  PPV = round(binconf(CM[1, 1], n1, alpha = 1 - conf.level,
                      method = "wilson"), digits)
  NPV = round(binconf(CM[2, 2], n2, alpha = 1 - conf.level,
                      method = "wilson"), digits)
  kappa = round(kappaCIboot(x, y, seed, num.boot, conf.level),
                digits)
  
  # Modify the printouts to have the right confidence interval quantiles
  colnames(Sensitivity)[2:3] <- colnames(Specificity)[2:3] <-
    colnames(PPV)[2:3] <- colnames(NPV)[2:3] <- 
    c(paste0((1 - conf.level) / 2 * 100, "%"),
      paste0((1 - (1 - conf.level) / 2) * 100, "%"))
  
  # Print results
  printCI <- function(z) {
    paste(z[1], "(", z[2], "-", z[3], ")")
  }
  
  cat(paste0("\n", "Accuracy: ", printCI(Accuracy), "\n", "Sensitivity: ",
             printCI(Sensitivity), "\n", "Specificity: ", printCI(Specificity),
             "\n", "PPV: ", printCI(PPV), "\n", "NPV: ", printCI(NPV), "\n",
             "kappa: ", printCI(kappa), "\n", "\n"))
  
  return(list(Accuracy = Accuracy, Sensitivity = Sensitivity,
              Specificity = Specificity, PPV = PPV, NPV = NPV, kappa = kappa))
}