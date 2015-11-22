library(dplyr)
types <- unname(sapply(emdb.all, class))
fd <- emdb.all[,types%in%c("factor", "numeric")]
type.fd <- unname(sapply(fd, class))

num.ind <- type.fd %in% c("numeric")
fac.ind <- type.fd %in% c("factor")
catvars <- colnames(fd)[fac.ind]
par(ask=T)
for (i in 1: length(catvars)){
print(i)
x= fd[, catvars[i]]
par(mfrow=c(2,1),mar=c(3.1,9.5,4.1,2.1))
wr.lap <- wrap.labels(names(table(x,useNA = "ifany")), 35)
barplot(prop.table(table(x, useNA = "ifany"))*100,
        horiz = T, las=2,names.arg = wr.lap, offset = 0,
        main=catvars[i], xlab = "%", cex.names = 0.5)
mat <- data.matrix(table(x,useNA = "ifany"));colnames(mat) <- "Freq"
gplots::textplot(mat)
}


# numvars=c("bmi")
# y= input.d[, numvars]
# boxplot(y~input.d[,"TMA"], main=numvars,las=2)
