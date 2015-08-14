#' Confusion matrix summaries
#' 
#' Calculates summaries from cross-tabulated reference and prediction labels
#' for a two-class variable.
#' 
#' Given two dichotomous variables summarized in a confusion matrix, this
#' function provides performance summaries. The accuracy, sensitivity,
#' specificity, positive predictive value (PPV), negative predictive value
#' (NPV), and the kappa statistic, along with their bootstrapped confidence
#' intervals are returned.
#' 
#' @param x a vector of predicted classes
#' @param y a vector of reference classes
#' @param seed random seed for bootstrapping
#' @param num.boot number of times to bootstrap. Defaults to 1000.
#' @param conf.level confidence level. Defaults to 95\%.
#' @param digits number of digits to round summaries to
#' @param verbose logical; if \code{TRUE}, outputs are printed to the screen
#' @return A confusion matrix for the predicted and reference classes. Then
#' the estimated statistics along with bootstrapped confidence intervals. A
#' list with the following elements
#' \item{Accuracy}{Accuracy point estimate, lower bound and upper bound for
#' bootstrapped CI}
#' \item{Sensitivity}{Sensitivity point estimate, lower bound and upper bound for
#' bootstrapped CI}
#' \item{Specificity}{Specificity point estimate, lower bound and upper bound for
#' bootstrapped CI}
#' \item{PPV}{PPV point estimate, lower bound and upper bound for
#' bootstrapped CI}
#' \item{NPV}{NPV point estimate, lower bound and upper bound for
#' bootstrapped CI}
#' \item{kappa}{kappa point estimate, lower bound and upper bound for
#' bootstrapped CI}
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples
#' ### 95% CI from 1000 bootstraped samples
#' set.seed(547)
#' n <- 80
#' x <- rbinom(n, size = 1, prob = 0.6)
#' y <- rbinom(n, size = 1, prob = 0.4)
#' confusionMatrix(x, y)
#' 
#' ### 90% CI from 500 bootstrapped samples
#' confusionMatrix(x, y, num.boot = 500, conf.level = 0.90)
#' 
#' ### Round to 2 digits
#' confusionMatrix(x, y, digits = 2)
confusionMatrix <- function(x, y, seed = 20, num.boot = 1000,
                            conf.level = 0.95, digits = 4,
                            verbose = FALSE) {
  CM <- table(Prediction = x, Reference = y)
  if (verbose) {
    cat("Confusion Matrix", "\n")
    print(CM)
  }
  # Compute marginal totals and correct predictions
  m1 <- CM[1, 1] + CM[2, 1]
  m2 <- CM[1, 2] + CM[2, 2]
  n1 <- CM[1, 1] + CM[1, 2]
  n2 <- CM[2, 1] + CM[2, 2]
  PO <- CM[1, 1] + CM[2, 2]
  
  # Calculate statistics and obtain confidence intervals
  Accuracy <- round(Hmisc::binconf(PO, m1 + m2, alpha = 1 - conf.level,
                           method = "wilson"), digits)
  Sensitivity <- round(Hmisc::binconf(CM[1, 1], m1, alpha = 1 - conf.level,
                              method = "wilson"), digits)
  Specificity <- round(Hmisc::binconf(CM[2, 2], m2, alpha = 1 - conf.level,
                              method = "wilson"), digits)
  PPV <- round(Hmisc::binconf(CM[1, 1], n1, alpha = 1 - conf.level,
                      method = "wilson"), digits)
  NPV <- round(Hmisc::binconf(CM[2, 2], n2, alpha = 1 - conf.level,
                      method = "wilson"), digits)
  kappa <- round(kappaBootCI(x, y, seed, num.boot, conf.level),
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
  
  if (verbose)
    cat(paste0("\n", "Accuracy: ", printCI(Accuracy), "\n", "Sensitivity: ",
               printCI(Sensitivity), "\n", "Specificity: ",
               printCI(Specificity), "\n", "PPV: ", printCI(PPV), "\n",
               "NPV: ", printCI(NPV), "\n", "kappa: ", printCI(kappa), "\n",
               "\n"))
  
  table <- matrix(c(Accuracy, Sensitivity, Specificity, PPV, NPV, kappa),
                  ncol = 3, byrow = T,
                  dimnames = list(c("Accuracy", "Sensitivity", "Specificity",
                                    "PPV", "NPV", "kappa"),
                                  c("Point Estimate", "Lower CI", "Upper CI")))
  table <- pander::pandoc.table.return(table)
  
  return(list(Accuracy = Accuracy, Sensitivity = Sensitivity,
              Specificity = Specificity, PPV = PPV, NPV = NPV, kappa = kappa,
              table = table))
}