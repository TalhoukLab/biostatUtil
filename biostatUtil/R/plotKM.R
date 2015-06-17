#' Plot Kaplan-Meier curve
#' @export
plotKM <- function(input.d,
                    input.formula,
                    main.text,
                    xlab.text,
                    ylab.text,
                    line.name,
                    line.color,
                    line.pattern=NULL,
                    line.width=NULL,
                    show.test="single",   # show single or the reference group value (for pairwise comparison)
                    # none = no test shown
                    single.test.type="logrank", # the test to show if specified show.test="single". 
                    # the possible choices are logrank, wilcoxon, taroneware, all
                    round.digits.p.value=4, # number of digits for p-value
                    obs.survyrs=10, # show the obs.survyrs (e.g. 10) survial rate on KM plot
                    legend.pos="bottomleft", # legend position keyword
                    file.name="no.file",
                    file.width=7,
                    file.height=7,
                    grey.scale=FALSE,
                    show.single.test.pos="default", # default: 0.5 if legend.pos="top" otherwise 0.1
                    ...) {
  
  # calculate "obs.survyrs"-yrs survival
  summary.surv.fit <- summary(survfit(input.formula, data=input.d), time=obs.survyrs, extend=T)
  
  n.cases <- table(input.d[,deparse(input.formula[[3]])]) # number of cases in each group variable = deparse(input.formula[[3]])
  decrement.count <- 0 # the number we need to take away from "i" because if n.cases[i]==0, length(summary.surv.fit) < length(line.name)
  
  ten.yrs.surv <- NULL
  for (i in 1:length(line.name)) {
    if (n.cases[i]==0) {
      # there must be no cases in this category
      ten.yrs.surv <- c(ten.yrs.surv,"NA")
      decrement.count <- decrement.count+1
    } else {
      ten.yrs.surv <- c(ten.yrs.surv,
                        paste(format(summary.surv.fit$surv*100, digits=3)[i-decrement.count],
                              "% (",
                              format(summary.surv.fit$lower*100, digits=3)[i-decrement.count],
                              "% - ",
                              format(summary.surv.fit$upper*100, digits=3)[i-decrement.count],
                              "%)", 
                              sep="")
      )
    }
  }                   
  
  # summary of survival object to end of followup
  fit.obj <- survfit(input.formula, data=input.d)		
  summary.surv.fit.all <- summary(fit.obj)[['table']]
  
  # NEED TO RESET decrement.count!!!!
  decrement.count <- 0 # the number we need to take away from "i" because if n.cases[i]==0, length(summary.surv.fit) < length(line.name)
  
  event.count <- NULL
  for (i in 1:length(line.name)) {
    if (n.cases[i]==0) {
      # there must be no cases in this category
      event.count <- c(event.count,"NA")
      decrement.count <- decrement.count+1
    } else {
      event.count <- c(event.count,
                       paste(summary.surv.fit.all[i-decrement.count,'events'],
                             "/",
                             summary.surv.fit.all[i-decrement.count,'records'],
                             sep="")
      )
    }
  }
  
  # determine show.single.test.pos
  if (show.single.test.pos=="default") {
    show.single.test.pos <- 0.1
    if (legend.pos=="top") {
      show.single.test.pos <- 0.5
    }
  }
  # plot km
  output <- plot_km_detail(input.d,
                           input.formula,
                           main.text, 
                           xlab.text, 
                           ylab.text,
                           line.name,
                           ten.yrs.surv,
                           event.count,
                           line.color,
                           obs.survyrs,
                           line.pattern=line.pattern,
                           line.width=line.width,
                           legend.pos=legend.pos,
                           file.name=file.name,
                           file.width=file.width,
                           file.height=file.height,
                           show.test=show.test,
                           round.digits.p.value=round.digits.p.value,
                           single.test.type=single.test.type,
                           grey.scale=grey.scale,
                           show.single.test.pos=show.single.test.pos,...
  )
  return(list(
    "log.rank.p.values"=output$log.rank.p.values,
    "wilcox.p.values"=output$wilcox.p.values,
    "n"=sum(fit.obj$n),
    "nevent"=sum(fit.obj$n.event)
  ))
}