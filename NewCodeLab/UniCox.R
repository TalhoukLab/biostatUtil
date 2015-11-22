library(biostatUtil)
library(gdata)
UniCox<- function(input.d,survTime,survStatus,survCode,var.name,
                  use.firth=0.8, digits=2, ref=NULL){

isf <- is.factor(input.d[,var.name])
lvlf<- nlevels(input.d[,var.name])
  
y.na <- is.na(input.d[, survStatus]) | is.na(input.d[, survTime])
x.na <- is.na(input.d[, var.name])
use.df <-input.d[!(y.na |x.na),]

surv.formula <- as.formula(paste0("Surv(", survTime, ", ", survStatus, "=='", survCode, "'  ) ~", var.name))
cox.stats  <- prettyCoxph(surv.formula, input.d = use.df , use.firth = use.firth)

# A multi-level factor
if(is.factor(use.df[,var.name])& nlevels(use.df[,var.name])>2){
nevent <- rep(cox.stats$nevent, (length(levels(use.df[,var.name]))-1))
n <- rep(cox.stats$n, (length(levels(use.df[,var.name]))-1))
res<- data.frame(predictor = rownames(cox.stats$output),
                 nevent, n, hr = round(as.numeric(cox.stats$output[, 1]), 2), 
                 lower=round(as.numeric(cox.stats$output[, 2]), 2), 
                 upper = round(as.numeric(cox.stats$output[, 3]), digits))
 
}else{

res<-data.frame(predictor=var.name,nevent=cox.stats$nevent,n=cox.stats$n,hr=round(as.numeric(cox.stats$output[1, 1]), 2), lower=round(as.numeric(cox.stats$output[2, 1]), 2),upper=round(as.numeric(cox.stats$output[3, 1]), 2))
}
return(res)
}


data(kidney)
head(kidney)


survTime= "time"
survStatus="status"
survCode="1"
var.name= "disease"
input.d= kidney
use.firth=0.8
digits=2

UniCox(input.d=kidney,survTime="time",survStatus="status",survCode="1",var.name="age")
