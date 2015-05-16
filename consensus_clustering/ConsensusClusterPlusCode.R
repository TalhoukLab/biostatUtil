require(ConsensusClusterPlus)
require(cluster)
require(bioDist)
source(paste(CodeCS3cons,"ConsensusClusterNMF.R",sep=""))

d=d.hgsc[!apply(d.hgsc,1,sd)<1,]
#d=data.matrix(sweep(d,1,apply(d,1,median,na.rm=T)))
d=t(scale(t(d),center=TRUE,scale=TRUE))

dianaHook = function(this_dist,k){
  tmp = diana(this_dist,diss=TRUE)
  assignment = cutree(tmp,k)
  return(assignment)  
}
agnesHook = function(this_dist,k){
  tmp = agnes(this_dist,diss=TRUE)
  assignment = cutree(tmp,k)
  return(assignment)  
}
myMIdist = function(x){ MIdist(t(d))}
mets=c("hcAEucl","hcDianaEucl","hcAgnesEucl","kmEucl","kmSpear","kmMI","pamEucl","pamSpear","pamMI")
D=format(Sys.Date(),format="%b%d")

hc.AL = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,title=paste(D,"hcAEucl",sep=""), plot="png", writeTable=TRUE)
hc.diana = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,title=paste(D,"hcDianaEucl",sep=""),clusterAlg="dianaHook", plot="png", writeTable=TRUE)
hc.agnes = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,title=paste(D,"hcAgnesEucl",sep=""),clusterAlg="agnesHook", plot="png", writeTable=TRUE)
km.eucl = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,distance="euclidean",clusterAlg="km",title=paste(D,"kmEucl",sep=""),plot="png", writeTable=TRUE)
km.spear = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,distance="spearman",clusterAlg="kmdist",title=paste(D,"kmSpear",sep=""),plot="png", writeTable=TRUE)
km.MI = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,distance="myMIdist",clusterAlg="kmdist",title=paste(D,"kmMI",sep=""),plot="png", writeTable=TRUE)
pam.eucl = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,distance="euclidean",clusterAlg="pam",title=paste(D,"pamEucl",sep=""),plot="png", writeTable=TRUE)
pam.spear = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,distance="spearman",clusterAlg="pam",title=paste(D,"pamSpear",sep=""),plot="png", writeTable=TRUE)
pam.MI = ConsensusClusterPlus(d,maxK=4,reps=r,pItem=p,pFeature=1,distance="myMIdist",clusterAlg="pam",title=paste(D,"pamMI",sep=""),plot="png", writeTable=TRUE)

#ConsensusClusterNMF(d,pItem=0.8,reps=500,k=4,DF)


