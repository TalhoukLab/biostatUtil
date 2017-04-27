#' Confusion matrix summaries
#' 
#' Calculates summaries from cross-tabulated reference and prediction labels for
#' a multi-class variable.
#' 
#' Given two multi-class variables summarized in a confusion matrix, this 
#' function provides performance summaries. It provides overall accuracy with 
#' confidence intervals, as well as per class accuracy, sensitivity, 
#' specificity, positive predictive value (PPV), negative predictive value (NPV).
#' if variable entered is binary, it will automatically call binaryCM
#' 
#' @param x a vector of reference classes
#' @param y a vector of predicted classes
#' @param seed a random seed for bootstrapping
#' @param num.boot the number of times to bootstrap. Defaults to 1000.
#' @param conf.level the confidence level. Defaults to 95\%.
#' @param digits the number of digits to round summaries to
#' @param method the method for obtaining confidence intervals for binomial 
#'   probabilities. See \code{Hmisc::binconf} for details.
#' @return A confusion matrix for the predicted and reference classes. Then the 
#'   estimated statistics along with bootstrapped confidence intervals. A
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
#' @family confusion matrix functions
#' @author Aline Talhouk, Derek Chiu
#' @export
#' @examples
#' ### 95% CI from 1000 bootstraped samples
#' set.seed(23)
#' k <- 3
#' (x <- sample(1:k, 100, replace = TRUE, prob = c(0.15, 0.25, 0.6)))
#' (y <- sample(1:k, 100, replace = TRUE, prob = c(0.05, 0.4, 0.65)))
#' prop.table(table(y))
#' multiClassCM(x, y)
#' 
#' ### 90% CI from 500 bootstrapped samples
#' multiClassCM(x, y, num.boot = 500, conf.level = 0.90)
#' 
#' ### Round to 2 digits
#' multiClassCM(x, y, digits = 2)
multiClassCM <- function(x, y, seed = 20, num.boot = 1000,
                         conf.level = 0.95, digits = 2,
                         method = "wilson") {
  CM <- table(Reference = as.character(x), Prediction = as.character(y))
  CMu <- addmargins(CM)
  
  #if(dim(CM)[1]<3){stop("This function only works for multi-class variables!")}
  if (!all(unique(x) %in% unique(y))) {
    stop("levels should be the same in the prediction and reference class!")
  }
  clm <- colSums(CM)
  rwm <- rowSums(CM)
  N <-  sum(CM)
  TP <- diag(CM)
  FP <- clm - TP
  FN <- rwm - TP
  TN <- N - (TP + FP + FN)
  
  # Overall----
  cc <- round(caret::confusionMatrix(y, x)$overall, digits = digits)
  ckappa <- round(kappaBootCI(x, y, seed, num.boot, conf.level), digits)
  
  # By class----
  acc <- (TP + TN) / N
  Accuracy <- round(Hmisc::binconf(TP + TN, N, alpha = 1 - conf.level,
                                   method = method), digits)  
  sens <- TP / clm
  Sensitivity <- round(Hmisc::binconf(TP, clm, alpha = 1 - conf.level,
                                      method = method), digits)
  
  spec <- TN / (N - clm)
  Specificity <- round(Hmisc::binconf(TN, (N - clm), alpha = 1 - conf.level,
                                      method = method), digits)
  
  ppv <- TP / rwm
  PPV <- round(Hmisc::binconf(TP, rwm, alpha = 1 - conf.level,
                              method = method), digits)
  
  npv <- TN / (N - rwm)
  NPV <- round(Hmisc::binconf(TN, (N - rwm), alpha = 1 - conf.level,
                              method = method), digits)
  prev <- clm / N
  Prevalence <- round(Hmisc::binconf(clm, N, alpha = 1 - conf.level,
                                     method = method), digits)
  detect <- TP / N
  Detection <- round(Hmisc::binconf(TP, N, alpha = 1 - conf.level,
                                    method = method), digits)
  detectPrev <- rwm / N
  DetectionPrev <- round(Hmisc::binconf(rwm, N, alpha = 1 - conf.level,
                                        method = method), digits)
  
  BA <- (sens + spec) / 2
  
  colnames(Sensitivity)[2:3] <- colnames(Specificity)[2:3] <-
    colnames(PPV)[2:3] <- colnames(NPV)[2:3] <- colnames(Prevalence)[2:3] <-
    colnames(Detection)[2:3] <- colnames(DetectionPrev)[2:3] <-
    c(paste0((1 - conf.level) / 2 * 100, "%"),
      paste0((1 - (1 - conf.level) / 2) * 100, "%"))
  
  overall <- 
    rbind("Overall Accuracy" = printCI(c(cc[1], cc[3], cc[4])),
          "Cohen's kappa" = printCI(ckappa),
          "No Information Rate" = cc[5],         
          "P-Value [Acc > NIR]" = cc[6]) %>% 
    set_colnames("Overall Concordance Statistics")
  
  table <- rbind("Sensitivity" = c(round(mean(sens),digits),
                                   apply(Sensitivity, 1, printCI)),
                 "Specificity" = c(round(mean(spec),digits), 
                                   apply(Specificity, 1, printCI)),
                 "Pos Pred Value" = c(round(mean(ppv), digits),
                                      apply(PPV, 1, printCI)),
                 "Neg Pred Value" = c(round(mean(npv), digits),
                                      apply(NPV, 1, printCI)),
                 "Prevalence" = c("",
                                  apply(Prevalence, 1, printCI)),
                 "Detection Rate" = c("",
                                      apply(Detection, 1, printCI)),
                 "Detection Prevalence" = c("",
                                            apply(DetectionPrev, 1, printCI)),
                 "Accuracy" = c(round(mean(acc),digits),
                                apply(Accuracy, 1, printCI)),
                 "Balanced Accuracy" = c(round(mean(BA),digits),
                                         round(BA, digits = digits))) %>% 
    set_colnames(c("Average", colnames(CM)))
  
  return(list(CM = CMu, overall = overall, table = table))
}
