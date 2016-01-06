#' Graphical Exploration of a Dataset
#'
#' Function to provide a graphical exploration of a dataset will print results to file.
#' @param datmat the data frame (only categorical and numerical variables will be analysed)
#' @author Aline Talhouk
#' @export
#'
#'

exploreData <- function(datmat){
types <- unname(sapply(datmat, class))
fd <- datmat[,types%in%c("factor", "numeric")]
type.fd <- unname(sapply(fd, class))

num.ind <- type.fd %in% c("numeric")
fac.ind <- type.fd %in% c("factor")
catvars <- colnames(fd)[fac.ind]
pdf("DataSummary.pdf")

for (i in 1: length(catvars)){
x= fd[, catvars[i]]
tx=table(x, useNA = "ifany")
par(mfrow=c(2,1),mar=c(3.1,9.5,4.1,2.1))
biostatUtil::barplotSum(tx,catvars[i])
mat <- data.matrix(cbind(tx,round(prop.table(tx)*100,1)));
colnames(mat) <- c("Freq","%")
rownames(mat)[is.na(rownames(mat))] <- "Missing"
tmat <- rbind(mat, apply(mat[,1:2],2,sum))
PerformanceAnalytics::textplot(tmat,wrap=F)
}


numvars <- colnames(fd)[num.ind]
for (i in 1: length(numvars)){
par(mfrow=c(2,1))
x= fd[, numvars[i]]
biostatUtil::boxplotSum(x,numvars[i])
biostatUtil::histSum(x)
}
dev.off()
}
