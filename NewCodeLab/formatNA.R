#input vector
#return factor or numeric and missing NA
formatNA <- function(x,type="cat"){
  x[x%in%c("","Unk","N/A",NA)] <- NA
  if(type=="cat"){
    res <- factor(x)
  }else{
      res <- as.numeric(x)}
return(res)
}

