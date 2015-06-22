
DSC<-function(data,Y){
  scatmat=scattermat(data,Y)
  DSC.observed=sqrt(sum(diag(scatmat$B)))/sqrt(sum(diag(scatmat$W)))
  
  number_of_permutations = 1000
  
  DSC.random = NULL
  for (i in 1 : number_of_permutations) {
    levs=levels(factor(Y))
    # Combine the two datasets into a single dataset
    # i.e., under the null hypothesis, there is no difference between the two groups
    # Sample from the combined dataset
    a.random = data[sample (dim(data)[1], sum(Y==levs[1]), TRUE),]
    b.random = data[sample (dim(data)[1], sum(Y==levs[2]), TRUE),]
    data.random=rbind(a.random,b.random)
    # Null (permuated) difference
    rand.scat=scattermat(data.random,Y)
    DSC.random[i]=sqrt(sum(diag(rand.scat$B)))/sqrt(sum(diag(rand.scat$W)))
  }
  # P-value is the fraction of how many times the permuted difference is equal or more extreme than the observed difference
  
  pvalue = sum(abs(DSC.random) >= abs(DSC.observed)) / number_of_permutations
  return(list(DSC=round(DSC.observed,3),pval=pvalue))
}
