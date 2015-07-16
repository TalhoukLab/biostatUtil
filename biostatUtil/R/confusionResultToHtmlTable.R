#' Confusion matrix results in HTML
#' @export
#########################################
# helper function to pring results from
# confusionMatrix as a html table
#
# num.boot.for.ci: number of bootstrap interval for CI.
# if boot.for.ci=NA, do not show CI
confusionResultToHtmlTable <- function(prediction, reference, ref.description,
                                       round.digits.p.value,
                                       num.boot.for.ci = NA, seed = 12) {
  td.right <- "<td style='text-align: right; white-space: nowrap;'>"
  td.left <- "<td style='text-align: left; white-space: nowrap;'>"
  
  # need to set prediction and reference as factors
  prediction <- as.factor(prediction)
  reference <- as.factor(reference)
  
  confusionResult <- confusionMatrix(prediction, reference)
  
  multi.levels <- !is.null(nrow(confusionResult$byClass)) # i.e. class with > 2 levels e.g. 3x3 table
  if (multi.levels) {
    confusionResult$byClass <- apply(confusionResult$byClass, 2, mean)
  }
  
  do.ci <- !is.na(num.boot.for.ci)
  boot.confusionResults <- NA
  if (!is.na(num.boot.for.ci)) {
    # do bootstrap confidence interval!!!
    boot.confusionResults <- as.data.frame(cbind(
      "kappa" = rep(NA,num.boot.for.ci),
      "sens" = rep(NA,num.boot.for.ci),
      "specs" = rep(NA,num.boot.for.ci),
      "ppv" = rep(NA,num.boot.for.ci),
      "npv" = rep(NA,num.boot.for.ci)))
    
    set.seed(seed)
    for (i in 1:num.boot.for.ci) {
      boot.indexes <- sample(1:length(prediction), replace = TRUE)
      #print(table(prediction[boot.indexes]))
      #print(table(reference[boot.indexes]))
      boot.confusionResult <- confusionMatrix(prediction[boot.indexes], reference[boot.indexes]) 
      boot.confusionResults$kappa[i] <- boot.confusionResult$overall[["Kappa"]]
      if (multi.levels) {
        boot.confusionResult$byClass <- apply(boot.confusionResult$byClass, 2, mean)
      }
      boot.confusionResults$sens[i]  <- boot.confusionResult$byClass["Sensitivity"]
      boot.confusionResults$specs[i] <- boot.confusionResult$byClass["Specificity"]
      boot.confusionResults$ppv[i]   <- boot.confusionResult$byClass["Pos Pred Value"]
      boot.confusionResults$npv[i]   <- boot.confusionResult$byClass["Neg Pred Value"]
    }
    boot.kappas <- sort(boot.confusionResults$kappa); boot.kappas <- boot.kappas[!is.na(boot.kappas)]
    boot.sens   <- sort(boot.confusionResults$sens);  boot.sens   <- boot.sens[  !is.na(boot.sens)]
    boot.specs  <- sort(boot.confusionResults$specs); boot.specs  <- boot.specs[ !is.na(boot.specs)]
    boot.ppvs   <- sort(boot.confusionResults$ppv);   boot.ppvs   <- boot.ppvs[  !is.na(boot.ppvs)]		
    boot.npvs   <- sort(boot.confusionResults$npv);   boot.npvs   <- boot.npvs[  !is.na(boot.npvs)]	
    kappa.ci <- boot.kappas[round(c(0.025, 0.975) * length(boot.kappas))]
    sens.ci  <- boot.sens[  round(c(0.025, 0.975) * length(boot.sens))]
    specs.ci <- boot.specs[ round(c(0.025, 0.975) * length(boot.specs))]
    ppv.ci   <- boot.ppvs[  round(c(0.025, 0.975) * length(boot.ppvs))]
    npv.ci   <- boot.npvs[  round(c(0.025, 0.975) * length(boot.npvs))]
  }
  
  return(paste(
    "<table>",
    "<tr>",td.right,"Reference:</td>",td.left,ref.description,
    "</td></tr>",
    "<tr>",td.right,"Accuracy (95%CI):</td>",td.left,
    round(confusionResult$overall[["Accuracy"]],digits=round.digits.p.value)," (",round(confusionResult$overall[["AccuracyLower"]],digits=round.digits.p.value)," - ",round(confusionResult$overall[["AccuracyUpper"]],digits=round.digits.p.value),")",
    "</td></tr>",
    "<tr>",td.right,"No Information Rate:</td>",td.left,
    round(confusionResult$overall[["AccuracyNull"]],digits=round.digits.p.value),
    "</td></tr>",
    "<tr>",td.right,"P-Value [Acc > NIR]:</td>",td.left,
    round(confusionResult$overall[["AccuracyPValue"]],digits=round.digits.p.value),
    "</td></tr>",
    
    "<tr><td> </td><td><td></tr>",
    
    "<tr>",td.right,"Kappa",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
    round(confusionResult$overall[["Kappa"]],digits=round.digits.p.value),
    ifelse(
      do.ci,
      paste(" (",round(kappa.ci[1],digits=round.digits.p.value),"-",round(kappa.ci[2],digits=round.digits.p.value),")",sep=""),
      ""
    ),
    "</td></tr>",
    "<tr>",td.right,"Mcnemar's Test P-Value:</td>",td.left,
    round(confusionResult$overall[["McnemarPValue"]],digits=round.digits.p.value),
    "</td></tr>",
    
    "<tr>",
    ifelse(!multi.levels ,"<td> </td><td><td>","<td style='text-align: left' colspan=2><i>note: the following are mean values across classes</i></td>"),
    "</tr>",
    
    "<tr>",td.right,"Sensitivity",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
    round(confusionResult$byClass["Sensitivity"],digits=round.digits.p.value),
    ifelse(
      do.ci,
      paste(" (",round(sens.ci[1],digits=round.digits.p.value),"-",round(sens.ci[2],digits=round.digits.p.value),")",sep=""),
      ""
    ),
    "</td></tr>",
    "<tr>",td.right,"Specificity",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
    round(confusionResult$byClass["Specificity"],digits=round.digits.p.value),
    ifelse(
      do.ci,
      paste(" (",round(specs.ci[1],digits=round.digits.p.value),"-",round(specs.ci[2],digits=round.digits.p.value),")",sep=""),
      ""
    ),
    "</td></tr>",
    "<tr>",td.right,"PPV",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
    round(confusionResult$byClass["Pos Pred Value"],digits=round.digits.p.value),
    ifelse(
      do.ci,
      paste(" (",round(ppv.ci[1],digits=round.digits.p.value),"-",round(ppv.ci[2],digits=round.digits.p.value),")",sep=""),
      ""
    ),
    "</td></tr>",
    "<tr>",td.right,"NPV",ifelse(do.ci," (95%CI)",""),":</td>",td.left,
    round(confusionResult$byClass["Neg Pred Value"],digits=round.digits.p.value),
    ifelse(
      do.ci,
      paste(" (",round(npv.ci[1],digits=round.digits.p.value),"-",round(npv.ci[2],digits=round.digits.p.value),")",sep=""),
      ""
    ),
    "</td></tr>",
    "<tr>",td.right,"Prevalence:</td>",td.left,
    round(confusionResult$byClass["Prevalence"],digits=round.digits.p.value),
    "</td></tr>",
    "<tr>",td.right,"Detection Rate:</td>",td.left,
    round(confusionResult$byClass["Detection Rate"],digits=round.digits.p.value),
    "</td></tr>",
    "<tr>",td.right,"Detection Prevalence:</td>",td.left,
    round(confusionResult$byClass["Detection Prevalence"],digits=round.digits.p.value),
    "</td></tr>",
    "<tr>",td.right,"Balanced Accuracy:</td>",td.left,
    round(confusionResult$byClass["Balanced Accuracy"],digits=round.digits.p.value),
    "</td></tr>",
    
    "</table>",
    sep=""
  ))
}