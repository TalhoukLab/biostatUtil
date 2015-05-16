ConsensusCluster<-function(xr,pItem,reps,k,OF){
# Function that generates multiple runs for consensusClustering (among samples and methods)
# x is a data matrix (genes are rows and samples are columns)
# pItem is the proportion of items to be used in the subsampling
# reps the number of repetitions to be done
# k is the number of clusters requested
# OF is the directory where the results will be written (at each iteration)

# load required packages  
require(cluster)
require(bioDist)
require(RColorBrewer)
require(NMF)

# tranform the data
x1=scale(xr[!apply(xr,1,sd)<1,])
samples=colnames(x1)
genes=rownames(x1)

#for nmf
pos=neg=x1
pos[which(x1<0)]=0;rownames(pos)=paste("pos",genes)
neg[which(x1>=0)]=0;rownames(neg)=paste("neg",genes)
x2=rbind(pos,-(neg),deparse.level = 0)

n=ncol(xr) #number of samples in original data
connect.matrix <- array(0, c( n, n,11))
coclus=array(NA, c(n,reps,11), dimnames=list(samples,paste("R",1:reps,sep=""),c("nmfDiv","nmfEucl","hcAEucl","hcDianaEucl","hcAgnesEucl","kmEucl","kmSpear","kmMI","pamEucl","pamSpear","pamMI")))

#rows are samples
#columns are Reps
#3rd D is method

n.new=floor(n*pItem)# subset the data
pb <- txtProgressBar(min = 0, max = reps, style = 3)

for (i in 1:reps){
  setTxtProgressBar(pb, i)
  ind.new=sample(n,n.new, replace=F)
 # x.new=x[,ind.new] #sample the rows with replacement
  #nmfDiv
 # nmf.x=x2[!(apply(x2[,ind.new],1,function(x)all(x==0))),ind.new]
 # coclus[ind.new,i,1]=predict(nmf(nmf.x,rank=k,method="brunet",seed=123456789))
 #nmfEucl
 # coclus[ind.new,i,2]=predict(nmf(nmf.x,rank=k,method="lee",seed=123456789))
  #hcAEucl
  coclus[ind.new,i,3]=cutree(hclust(euc(t(x1[,ind.new])),method="average"),k )
  #hcDianaEucl
  #coclus[ind.new,i,4]=cutree(diana(euc(t(x1[,ind.new])),diss=TRUE),k )
  #hcAgnesEucl
  #coclus[ind.new,i,5]=cutree(agnes(euc(t(x1[,ind.new])),diss=TRUE),k )
  #kmeans Euclidean
  #coclus[ind.new,i,6]=kmeans(euc(t(x1[,ind.new])),k)$cluster
  #kmeans Spearman
  #coclus[ind.new,i,7]=kmeans(spearman.dist(t(x1[,ind.new])),k)$cluster
  #kmeans MI
  coclus[ind.new,i,8]=kmeans(MIdist(t(x1[,ind.new])),k)$cluster
  #pamEucl
  #coclus[ind.new,i,9]=pam(euc(t(x1[,ind.new])),k,cluster.only=TRUE)
  #pamSpear
  #coclus[ind.new,i,10]=pam(spearman.dist(t(x1[,ind.new])),k,cluster.only=TRUE)
  #pamMI
  #coclus[ind.new,i,11]=pam(spearman.dist(t(x1[,ind.new])),k,cluster.only=TRUE)
}
saveRDS(coclus, paste(OF,"test.rds",sep=""))

}