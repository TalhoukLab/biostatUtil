ConsensusClusterNMF<-function(xr,pItem,reps,k,OF,mets=c("nmfDiv","nmfEucl")){
# Function that generates multiple runs for consensusClustering (among samples)
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
samples=colnames(xr)
genes=rownames(xr)

#for nmf
pos=neg=xr
pos[which(xr<0)]=0;rownames(pos)=paste("pos",genes)
neg[which(xr>=0)]=0;rownames(neg)=paste("neg",genes)
x2=rbind(pos,-(neg),deparse.level = 0)
#mets=c("nmfDiv","nmfEucl","hcAEucl","hcDianaEucl","hcAgnesEucl","kmEucl","kmSpear","kmMI","pamEucl","pamSpear","pamMI")
m=length(mets)
n=ncol(xr) #number of samples in original data
connect.matrix <- array(0, c( n, n,m))
coclus=array(NA, c(n,reps,m), dimnames=list(samples,paste("R",1:reps,sep=""),mets))

#rows are samples
#columns are Reps
#3rd D is method

n.new=floor(n*pItem)# subset the data
pb <- txtProgressBar(min = 0, max = reps, style = 3)

for (i in 1:reps){
  setTxtProgressBar(pb, i)
  ind.new=sample(n,n.new, replace=F)
#  # x.new=x[,ind.new] #sample the rows with replacement
#   #nmfDiv
   nmf.x=x2[!(apply(x2[,ind.new],1,function(x)all(x==0))),ind.new]
   coclus[ind.new,i,1]=predict(nmf(nmf.x,rank=k,method="brunet",seed=123456789))
#  #nmfEucl
   coclus[ind.new,i,2]=predict(nmf(nmf.x,rank=k,method="lee",seed=123456789))
saveRDS(coclus, paste(OF,"outputApr18.rds",sep=""))

}

}