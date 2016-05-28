#' Confusion matrix results in HTML
#' 
#' Prints results from \code{binaryCM} into a nice HTML table format.
#' @param prediction vector of predicted classes
#' @param reference vector of reference classes
#' @param ref.description description of classes
#' @param seed random seed for bootstrap resampling
#' @param num.boot number of bootstrap confidence intervals.
#' If \code{num.boot = NA}, do not show CI.
#' @param conf.level confidence level. Defaults to 95\%.
#' @param digits number of digits to round p-values to
#' @author Samuel Leung, Derek Chiu
#' @export
#' @examples 
#' # 95% CI from 10 bootstraped samples
#' library(htmlTable)
#' set.seed(547)
#' n <- 80
#' x <- rbinom(n, size = 1, prob = 0.6)
#' y <- rbinom(n, size = 1, prob = 0.4)
#' results <- confusionResultToHtmlTable(x, y, "Test", num.boot = 10)
#' htmlTable(results)
confusionResultToHtmlTable <- function(prediction, reference, ref.description,
                                       seed = 20, num.boot = NA,
                                       conf.level = 0.95, digits = 4) {
  td.right <- "<td style='text-align: right; white-space: nowrap;'>"
  td.left <- "<td style='text-align: left; white-space: nowrap;'>"
  do.ci <- !is.na(num.boot)
  ci.label <- ifelse(do.ci, paste0(" (", conf.level * 100, "% CI):"), "")
  ci <- c((1 - conf.level) / 2, (1 - (1 - conf.level) / 2))
  prediction <- as.factor(prediction)
  reference <- as.factor(reference)
  confusionResult <- binaryCM(prediction, reference)
  boot.confusionResults <- NA
  if (do.ci) {
    boot.confusionResults <- as.data.frame(cbind(
      "kappa" = rep(NA, num.boot),
      "sens" = rep(NA, num.boot),
      "specs" = rep(NA, num.boot),
      "ppv" = rep(NA, num.boot),
      "npv" = rep(NA, num.boot)))
    for (i in 1:num.boot) {
      index <- sample(1:length(prediction), replace = TRUE)
      boot.result <- binaryCM(prediction[index], reference[index],
                              seed = NULL)
      boot.confusionResults$kappa[i] <- boot.result$kappa[1]
      boot.confusionResults$sens[i] <- boot.result$Sensitivity[1]
      boot.confusionResults$specs[i] <- boot.result$Specificity[1]
      boot.confusionResults$ppv[i] <- boot.result$PPV[1]
      boot.confusionResults$npv[i] <- boot.result$NPV[1]
    }
    boot.kappas <- sort(boot.confusionResults$kappa)
    boot.sens <- sort(boot.confusionResults$sens)
    boot.specs <- sort(boot.confusionResults$specs)
    boot.ppvs <- sort(boot.confusionResults$ppv)
    boot.npvs <- sort(boot.confusionResults$npv)
    kappa.ci <- boot.kappas[ceiling(ci * length(boot.kappas))]
    sens.ci <- boot.sens[ceiling(ci * length(boot.sens))]
    specs.ci <- boot.specs[ceiling(ci * length(boot.specs))]
    ppv.ci <- boot.ppvs[ceiling(ci * length(boot.ppvs))]
    npv.ci <- boot.npvs[ceiling(ci * length(boot.npvs))]
  }
  
  return(paste0(
    "<table>",
    "<tr>", td.right, "Reference:</td>", td.left, ref.description,
    "</td></tr>",
    "<tr>", td.right, "Accuracy", ci.label, "</td>", td.left,
    round(confusionResult$Accuracy[1], digits = digits), " (",
    round(confusionResult$Accuracy[2], digits = digits), " - ",
    round(confusionResult$Accuracy[3], digits = digits), ")",
    "</td></tr>",
    "<tr><td> </td><td><td></tr>",
    "<tr>", td.right, "Kappa", ci.label, "</td>", td.left,
    round(confusionResult$kappa[1], digits = digits),
    ifelse(do.ci, paste0(" (", round(kappa.ci[1], digits = digits), " - ",
                         round(kappa.ci[2], digits = digits), ")"), ""),
    "</td></tr>",
    "<tr>", td.right, "Sensitivity", ci.label, "</td>", td.left,
    round(confusionResult$Sensitivity[1], digits = digits),
    ifelse(do.ci, paste0(" (", round(sens.ci[1], digits = digits), " - ",
                         round(sens.ci[2], digits = digits), ")"), ""),
    "</td></tr>",
    "<tr>", td.right, "Specificity", ci.label, "</td>", td.left,
    round(confusionResult$Specificity[1], digits = digits),
    ifelse(do.ci, paste0(" (", round(specs.ci[1], digits = digits), " - ",
                         round(specs.ci[2], digits = digits), ")"), ""),
    "</td></tr>",
    "<tr>", td.right, "PPV", ci.label, "</td>", td.left,
    round(confusionResult$PPV[1], digits = digits),
    ifelse(do.ci, paste0(" (", round(ppv.ci[1], digits = digits), " - ",
                         round(ppv.ci[2], digits = digits), ")"), ""),
    "</td></tr>",
    "<tr>", td.right, "NPV", ci.label, "</td>", td.left,
    round(confusionResult$NPV[1], digits = digits),
    ifelse(do.ci, paste0(" (", round(npv.ci[1], digits = digits), " - ",
                         round(npv.ci[2], digits = digits), ")"),  ""),
    "</td></tr>",
    "</table>"))
}