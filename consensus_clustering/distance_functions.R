## Distance Functions
library(cluster)
library(bioDist)

# DIvisive ANAlysis Clustering
dianaHook = function(this_dist,k){
  tmp = diana(this_dist,diss=TRUE)
  assignment = cutree(tmp,k)
  return(assignment)  
}

# Agglomerative Nesting
agnesHook = function(this_dist,k){
  tmp = agnes(this_dist,diss=TRUE)
  assignment = cutree(tmp,k)
  return(assignment)  
}

# Mutual Information
myMIdist = function(x) { 
  MIdist(t(d))
}