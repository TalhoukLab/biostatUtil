#' Confusion matrix results in HTML
#' 
#' Prints results from \code{confusionMatrix} into a nice HTML table format.
#' @param prediction vector of predicted classes
#' @param reference vector of reference classes
#' @param ref.description description of classes
#' @param digits number of digits to round p values
#' @param num.boot number of bootstrap confidence intervals.
#' If \code{num.boot = NA}, do not show CI.
#' @param seed random seed for bootstrap resampling
#' @author Samuel Leung
#' @export
#' @examples 
#' ### 95% CI from 1000 bootstraped samples
#' set.seed(547)
#' n <- 80
#' x <- rbinom(n, size = 1, prob = 0.6)
#' y <- rbinom(n, size = 1, prob = 0.4)
#' confusionResultToHtmlTable(x, y, "")
confusionResultToHtmlTable <- function(prediction, reference, ref.description,
                                       digits = 4, num.boot = NA, seed = 20) {
  td.right <- "<td style='text-align: right; white-space: nowrap;'>"
  td.left <- "<td style='text-align: left; white-space: nowrap;'>"
  
  prediction <- as.factor(prediction)
  reference <- as.factor(reference)
  confusionResult <- confusionMatrix(prediction, reference)
  
  do.ci <- !is.na(num.boot)
  boot.confusionResults <- NA
  if (!is.na(num.boot)) {
    boot.confusionResults <- as.data.frame(cbind(
      "kappa" = rep(NA, num.boot),
      "sens" = rep(NA, num.boot),
      "specs" = rep(NA, num.boot),
      "ppv" = rep(NA, num.boot),
      "npv" = rep(NA, num.boot)))
    
    set.seed(4)
    for (i in 1:num.boot) {
      index <- sample(1:length(prediction), replace = TRUE)
      boot.result <- confusionMatrix(prediction[index], reference[index])
      boot.confusionResults$kappa[i] <- boot.result$kappa[1]
      boot.confusionResults$sens[i] <- boot.result$Sensitivity[1]
      boot.confusionResults$specs[i] <- boot.result$Specificity[1]
      boot.confusionResults$ppv[i] <- boot.result$PPV[1]
      boot.confusionResults$npv[i] <- boot.result$NPV[1]
    }
    boot.kappas <- sort(boot.confusionResults$kappa)
    boot.sens   <- sort(boot.confusionResults$sens)
    boot.specs  <- sort(boot.confusionResults$specs)
    boot.ppvs   <- sort(boot.confusionResults$ppv)
    boot.npvs   <- sort(boot.confusionResults$npv)
    kappa.ci <- boot.kappas[round(c(0.025, 0.975) * length(boot.kappas))]
    sens.ci  <- boot.sens[round(c(0.025, 0.975) * length(boot.sens))]
    specs.ci <- boot.specs[round(c(0.025, 0.975) * length(boot.specs))]
    ppv.ci   <- boot.ppvs[round(c(0.025, 0.975) * length(boot.ppvs))]
    npv.ci   <- boot.npvs[round(c(0.025, 0.975) * length(boot.npvs))]
  }
  
  return(paste0(
    "<table>",
    "<tr>", td.right, "Reference:</td>", td.left, ref.description,
    "</td></tr>",
    "<tr>", td.right, "Accuracy (95%CI):</td>", td.left,
    round(confusionResult$Accuracy[1],
          digits = digits), " (",
    round(confusionResult$Accuracy[2],
          digits = digits), " - ",
    round(confusionResult$Accuracy[3],
          digits = digits), ")",
    "</td></tr>",
    
    "<tr><td> </td><td><td></tr>",
    
    "<tr>", td.right, "Kappa", ifelse(do.ci, " (95%CI)", ""),
    ":</td>", td.left,
    round(confusionResult$kappa[1], digits = digits),
    ifelse(
      do.ci,
      paste0(" (", round(kappa.ci[1], digits = digits), "-",
             round(kappa.ci[2], digits = digits), ")"),
      ""
    ),
    "</td></tr>",
    "<tr>", td.right, "Sensitivity", ifelse(do.ci, " (95%CI)", ""),
    ":</td>", td.left,
    round(confusionResult$Sensitivity[1],
          digits = digits),
    ifelse(
      do.ci,
      paste0(" (", round(sens.ci[1], digits = digits), "-",
             round(sens.ci[2], digits = digits), ")"),
      ""
    ),
    "</td></tr>",
    "<tr>", td.right, "Specificity", ifelse(do.ci, " (95%CI)", ""),
    ":</td>", td.left,
    round(confusionResult$Specificity[1],
          digits = digits),
    ifelse(
      do.ci,
      paste0(" (", round(specs.ci[1], digits = digits), "-",
            round(specs.ci[2], digits = digits), ")"),
      ""
    ),
    "</td></tr>",
    "<tr>", td.right, "PPV", ifelse(do.ci, " (95%CI)", ""),
    ":</td>", td.left,
    round(confusionResult$PPV[1],
          digits = digits),
    ifelse(
      do.ci,
      paste0(" (", round(ppv.ci[1], digits = digits), "-",
             round(ppv.ci[2], digits = digits), ")"),
      ""
    ),
    "</td></tr>",
    "<tr>", td.right, "NPV", ifelse(do.ci, " (95%CI)", ""),
    ":</td>", td.left,
    round(confusionResult$NPV[1],
          digits = digits),
    ifelse(
      do.ci,
      paste0(" (", round(npv.ci[1], digits = digits), "-",
            round(npv.ci[2], digits = digits), ")"),
      ""
    ),
    "</td></tr>",
    "</table>"))
}