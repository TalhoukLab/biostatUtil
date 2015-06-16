#' ggkm
#' @export
ggkm <- function(sfit,sfit2=NULL,
                 table = TRUE,
                 returns = FALSE,
                 marks=TRUE,
                 cols=rainbow(100),
                 shading.colors=c("blue2","red2","deepskyblue","indianred3"),
                 xlabs = "Time",
                 ylabs = "Survival Probability",
                 xlims = c(0,max(sfit$time)),
                 ylims = c(0,1),
                 ystratalabs = NULL,
                 ystrataname = NULL,
                 timeby = 5,
                 main = "Kaplan-Meier Plot",
                 pval = TRUE,
                 HR=TRUE,
                 use.firth=1,  # specify threshold of event rate to use Firth correction; 1 means NEVER, -1 means ALWAYS
                 CI=TRUE,
                 subs = NULL,
                 legend=FALSE,
                 ...) {
  
  #############
  # libraries #
  #############
  require(ggplot2)
  require(survival)
  require(gridExtra)
  require(reshape) #rbind.fill function is in this package.
  require(plyr) #additional dependency.
  
  # some local variables (may be they should be parameters?)
  line.y.increment <- 0.05 # for annotate(), to indicate the much y should be incremented for each line
  
  # sorting the use of subsetting ----
  times <- seq(0, max(sfit$time), by = timeby)
  if(is.null(subs)){
    subs1 <- 1:length(levels(summary(sfit)$strata))
    subs2 <- 1:length(summary(sfit,censored=T)$strata)
    subs3 <- 1:length(summary(sfit,times = times,extend = TRUE)$strata)
  } else{
    for(i in 1:length(subs)){
      if(i==1){
        ssvar <- paste("(?=.*\\b=",subs[i],sep="")
      }
      if(i==length(subs)){
        ssvar <- paste(ssvar,"\\b)(?=.*\\b=",subs[i],"\\b)",sep="")	
      }
      if(!i %in% c(1, length(subs))){
        ssvar <- paste(ssvar,"\\b)(?=.*\\b=",subs[i],sep="")	
      }
      if(i==1 & i==length(subs)){
        ssvar <- paste("(?=.*\\b=",subs[i],"\\b)",sep="")
      }
    }
    subs1 <- which(regexpr(ssvar,levels(summary(sfit)$strata), perl=T)!=-1)
    subs2 <- which(regexpr(ssvar,summary(sfit,censored=T)$strata, perl=T)!=-1)
    subs3 <- which(regexpr(ssvar,summary(sfit,times = times,extend = TRUE)$strata, perl=T)!=-1)
  }
  
  if( !is.null(subs) ) pval <- FALSE
  
  
  # data manipulation pre-plotting ----
  if(is.null(ystratalabs)) ystratalabs <- as.character(sub("group=*","",names(sfit$strata))) 
  if(is.null(ystrataname)) ystrataname <- "Strata"
  m <- max(nchar(ystratalabs))
  times <- seq(0, max(sfit$time), by = timeby)
  
  .df <- data.frame(                      # data to be used in the survival plot
    time = sfit$time[subs2],
    n.risk = sfit$n.risk[subs2],
    n.event = sfit$n.event[subs2],
    n.censor = sfit$n.censor[subs2],
    surv = sfit$surv[subs2],
    strata = factor(summary(sfit, censored = T)$strata[subs2]),
    upper = sfit$upper[subs2],
    lower = sfit$lower[subs2]
  )
  levels(.df$strata) <- ystratalabs       # final changes to data for survival plot
  zeros <- data.frame(time = 0, surv = 1,
                      strata = factor(ystratalabs, levels=levels(.df$strata)),
                      upper = 1, lower = 1)
  .df <- rbind.fill(zeros, .df)
  d <- length(levels(.df$strata))
  
  # specifying plot parameteres etc ----
  p<- (ggplot(.df , aes(time,surv,color=strata,fill=strata)) 
       + geom_step(aes(color = strata), size = .7)
       + theme_bw() + scale_colour_manual(values=shading.colors) + scale_fill_manual(values=shading.colors)
       + theme(axis.title.x = element_text(vjust = 0.5)) 
       + scale_x_continuous(xlabs, breaks = times, limits = xlims) 
       + scale_y_continuous(ylabs, limits = ylims) 
       + theme(panel.background = element_blank(), panel.grid.minor=element_blank(), panel.grid.major=element_blank())
       +theme(plot.margin = unit(c(0, 1, .5,ifelse(m < 10, 1.5, 2.5)),"lines")) 
       +ggtitle(main))
  
  #Legend----
  if(legend==TRUE){
    p<-p+theme(legend.position = c(.8,.88)) +    # MOVE LEGEND HERE [first is x dim, second is y dim]
      theme(legend.key = element_rect(colour = NA)) +
      theme(legend.title = element_blank()) 
  }else{
    p<-p + theme(legend.position="none")
  }
  
  #Confidence Bands----  
  if(CI==TRUE){
    p<-p + geom_ribbon(data=.df, aes(ymin = lower,ymax=upper), alpha=0.05,linetype=0) 
  }
  
  #Censor Marks----
  if(marks == TRUE){
    p <- p + geom_point(data = subset(.df, n.censor >= 1), 
                        aes(x = time, y = surv), shape = "/",size=4)
  }  
  
  ## Create a blank plot for place-holding
  blank.pic <- ggplot(.df, aes(time, surv)) + geom_blank() + theme_bw() +
    theme(axis.text.x = element_blank(),axis.text.y = element_blank(),
          axis.title.x = element_blank(),axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank(),panel.border = element_blank())
  
  
  # p-value placement----
  if(is.null(sfit2)){
    if(pval) {
      sdiff <- survdiff(eval(sfit$call$formula), data = eval(sfit$call$data))
      pval <- pchisq(sdiff$chisq,length(sdiff$n) - 1,lower.tail = FALSE)
      pvaltxt <- ifelse(pval < 0.001,"Log Rank p < 0.001",paste("Log Rank p =", signif(pval, 3)))
      if(HR) {
        #coxm<-coxph(eval(sfit$call$formula), data = eval(sfit$call$data))
        pretty.coxph.obj <- pretty.coxph(eval(sfit$call$formula), input.d= eval(sfit$call$data), use.firth=use.firth)
        if (pretty.coxph.obj$used.firth) {
          coxm<-pretty.coxph.obj$fit.firth
          HRtxts <- Xunivcoxph(coxm,coxph.type="coxphf")
        } else {
          coxm<-pretty.coxph.obj$fit
          HRtxts <- Xunivcoxph(coxm)
        }
        show.ref.group <- length(HRtxts)>1
        for (i in 1:length(HRtxts)) {
          HRtxt <- HRtxts[i]
          if (show.ref.group) {
            HRtxt <- paste(HRtxt," ~ ",ystratalabs[i+1]," vs. ",ystratalabs[1],sep="")
          }
          p <- p + annotate("text",x = 0.2 * max(sfit$time), hjust = 0, y = 0.01+line.y.increment*i,label = HRtxt, size=3)					
        }
      }
    }
    p <- p + annotate("text",x = 0.2 * max(sfit$time), hjust = 0, y = 0.01,label = ifelse(pval,pvaltxt,""), size=3)
  } else{
    if(pval) {
      sdiff <- survdiff(eval(sfit2$call$formula), data = eval(sfit2$call$data))
      pval <- pchisq(sdiff$chisq,length(sdiff$n) - 1,lower.tail = FALSE)
      pvaltxt <- ifelse(pval < 0.001,"Log Rank p < 0.001",paste("Log Rank p =", signif(pval, 3)))
      if(HR) {
        #coxm<-coxph(eval(sfit2$call$formula), data = eval(sfit2$call$data))
        pretty.coxph.obj <- pretty.coxph(eval(sfit2$call$formula), input.d= eval(sfit2$call$data), use.firth=use.firth)
        if (pretty.coxph.obj$used.firth) {
          coxm<-pretty.coxph.obj$fit.firth
          HRtxts <- Xunivcoxph(coxm,coxph.type="coxphf")
        } else {
          coxm<-pretty.coxph.obj$fit
          HRtxts <- Xunivcoxph(coxm)
        }
        show.ref.group <- length(HRtxts)>1
        for (i in 1:length(HRtxts)) {
          HRtxt <- HRtxts[i]
          if (show.ref.group) {
            HRtxt <- paste(HRtxt," ~ ",ystratalabs[i+1]," vs. ",ystratalabs[1],sep="")
          }
          p <- p + annotate("text",x = 0.2 * max(sfit2$time), hjust = 0, y = 0.01+line.y.increment*i,label = HRtxt, size=3)					
        }
      }
    }
    
    p <- p + annotate("text",x = 0.2 * max(sfit2$time),y = 0.01,label = ifelse(pval,pvaltxt,""), size=3)
    
  }
  
  # Create table graphic to include at-risk numbers----
  
  if(table) {
    risk.data <- data.frame(
      strata = factor(summary(sfit,times = times,extend = TRUE)$strata[subs3],),
      time = as.numeric(summary(sfit,times = times,extend = TRUE)$time[subs3])*0.97+xlims[2]*0.02, # doing some nasty empirical scaling/shifting here!!!  remove scaling/shifting factors if dosen't look right
      n.risk = summary(sfit,times = times,extend = TRUE)$n.risk[subs3]
    )
    risk.data$strata <- factor(risk.data$strata, levels=rev(levels(risk.data$strata)))
    
    data.table <- ggplot(risk.data,aes(x = time, y = strata, label = format(n.risk, nsmall = 0))) +
      #, color = strata)) +
      geom_text(size = 3.5) + theme_bw() +
      scale_y_discrete(breaks = as.character(levels(risk.data$strata)),
                       labels = rev(ystratalabs)) +
      scale_x_continuous("Numbers at risk", limits = xlims) +
      theme(axis.title.x = element_text(size = 12, vjust = 1),
            panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),axis.text.x = element_blank(),
            axis.ticks = element_blank(),axis.text.y = element_text(face = "bold", color=rev(shading.colors[1:length(ystratalabs)]), hjust = 1))
    
    data.table <- data.table +
      theme(legend.position = "none") + xlab(NULL) + ylab(NULL)
    
    data.table <- data.table +
      theme(plot.margin = unit(c(-1.5, 1, 0.1, ifelse(m < 10, 2.5, 3.5) - 0.28 * m), "lines")) # ADJUST POSITION OF TABLE FOR AT RISK
    
    #######################
    # Plotting the graphs #
    #######################
    grid.arrange(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                 ncol = 1, heights = unit(c(2, .1, .25),c("null", "null", "null")))
    
    if(returns) {
      a <- arrangeGrob(p, blank.pic, data.table, clip = FALSE, nrow = 3,
                       ncol = 1, heights = unit(c(2, .1, .25), c("null", "null", "null")))
      return(a)
    }
  } else {
    
    if(returns) return(p)
  }
}