context("Cox proportional hazard model engine (prettyCoxph)")
library(survival)
# generate a data data set for testing -----------------------------------------
if (exists("test.d")) BAK.TEST.D <- test.d # someone may have used this variable
test.d <- (function(){
  set.seed(12)
  n <- 200 # number of cases
  data.frame(
    os.yrs=sample(1:1000,size=n,replace=TRUE)/10,
    os.sts=sample(c("os.censored","os.event"),size=n,replace=TRUE),
    age=sample(30:90,size=n,replace=TRUE),
    grade=sample(1:3,size=n,replace=TRUE),
    stringsAsFactors=FALSE
  )
})()

surv_formula_1 <- Surv(os.yrs, os.sts == "os.event") ~ age + grade
fit_coxph <- coxph(surv_formula_1,data=test.d)
fit_prettyCoxph <- prettyCoxph(surv_formula_1,test.d) # default not use firth
test_that("prettyCoxph returns values from survival::coxph when use.firth==1",
  expect_equal(fit_coxph$n,fit_prettyCoxph$n)
)






















# clean up ---------------------------------------------------------------------
if (exists("BAK.TEST.D")) {
  test.d <- BAK.TEST.D
}