#' Do a jitterplot among subtypes
#' @export
doJitterplotAmongSubtypes <- function(input.d,data.description,biomarker.var.name,biomarker.name,subtype.var.name,subtype.name,pch=".",jitter=0.05,digits=3,cex.axis=0.9) {
  
  temp.d <- input.d[
    (!is.na(input.d[,biomarker.var.name])) & 
      (!is.na(input.d[,subtype.var.name])),
    ]
  
  biomarker <- temp.d[,biomarker.var.name]
  subtype <- temp.d[,subtype.var.name]
  xbar <- tapply(biomarker,subtype,boot.mean)
  
  test.name <- "Kruskal-Wallis"
  p.value <- kruskal.test(biomarker~subtype)$p.value
  if (length(names(table(subtype)))==2) {
    test.name <- "Wilcoxon Rank Sum"
    p.value <- wilcox.test(biomarker~subtype)$p.value
  }
  par(mar=c(5.1, # bottom margin
            4.1,
            5.1, # top
            2.1))
  stripchart(
    biomarker~subtype,
    method="jitter",
    jitter=jitter,
    pch=pch,
    group.names=paste(
      paste(
        names(xbar),
        rep("\nn=",length(xbar)),
        sep=""
      ),
      sapply(xbar,function(x){return(x$n)},USE.NAMES=FALSE),
      sep=""
    ),
    ylab=biomarker.name,
    xlab=subtype.name,
    main=paste(
      data.description,
      "\n",test.name," test P=",format(p.value,digits=digits),
      sep=""
    ),
    cex.axis=cex.axis,
    vert=TRUE)
  arrows(
    1:length(xbar), 
    sapply(xbar,function(x){return(x$ci[1])},USE.NAMES=FALSE),
    1:length(xbar),
    sapply(xbar,function(x){return(x$ci[2])},USE.NAMES=FALSE),
    angle=90,code=3,length=0.1)
  points(
    sapply(xbar,function(x){return(x$obs.mean)},USE.NAMES=FALSE),
    pch=4, # this is the X symbol
    type="p",
    cex=2)
}