datmat<-emdb.all

types <- unname(sapply(datmat, class))
fd <- datmat[,types%in%c("factor", "numeric")]
type.fd <- unname(sapply(fd, class))

num.ind <- type.fd %in% c("numeric")
fac.ind <- type.fd %in% c("factor")
catvars <- colnames(fd)[fac.ind]

for (i in 1: length(catvars)){
print(i)
x= fd[, catvars[i]]
tx=table(x, useNA = "ifany")
par(mfrow=c(2,1),mar=c(3.1,9.5,4.1,2.1))
wr.lap <- wrap.labels(names(table(x,useNA = "ifany")), 35)
barplot(prop.table(tx)*100,
        horiz = T, las=2,names.arg = wr.lap, offset = 0,
        main=catvars[i], xlab = "%", cex.names = 0.5, col="lightblue")
mat <- data.matrix(cbind(tx,round(prop.table(tx)*100,1)));colnames(mat) <- c("Freq","%")
tmat <- rbind(mat, apply(mat[,1:2],2,sum))
PerformanceAnalytics::textplot(tmat,wrap=F)
}


numvars <- colnames(fd)[num.ind]
for (i in 1: length(numvars)){
print(i)
par(mfrow=c(2,1))
x= fd[, numvars[i]]
boxplotSum(x,numvars[i])
histSum(x)
}
