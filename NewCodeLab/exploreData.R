library(dplyr)
fd <- emdb.all %>% select(TMA,tissue.source,patientID,Date.of.Surgery, Method.of.Initial.Pathologic.Diagnosis,Surgical.Approach, Surgical.Procedures)
str(fd)
types <- unname(sapply(emdb.all, class))
fd <- emdb.all[,types%in%c("factor", "logical", "numeric","integer")]
type.fd <- unname(sapply(fd, class))

num.ind <- type.fd %in% c("numeric", "integer")
fac.ind <- type.fd %in% c("factor","logical")
catvars <- colnames(fd)[fac.ind]
par(ask=T)
for (i in 1: length(catvars)){
print(i)
x= fd[, catvars[i]]
par(mfrow=c(2,1),mar=c(3.1,7.5,4.1,2.1))
wr.lap <- wrap.labels(names(table(x,useNA = "ifany")), 20)
barplot(prop.table(table(x, useNA = "ifany"))*100,
        horiz = T, las=2,names.arg = wr.lap, offset = 0,
        main=catvars[i], xlab = "%", cex.names = 0.5)
gplots::textplot(data.frame(table(x,useNA = "ifany")))
}


numvars=c("bmi")
y= input.d[, numvars]
boxplot(y~input.d[,"TMA"], main=numvars,las=2)
