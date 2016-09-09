context("Testing doCox")
test1 <- data.frame(time=c(4,3,1,1,2,2,3),
              status=c(1,1,1,0,1,1,0),
              x=c(0,2,1,1,1,0,0),
              sex=c(0,0,0,0,1,1,1))
formula <- survival::Surv(time, status) ~ x + sex
data <- test1

test_that("error if not fed an array",{
expect_error(doCox(formula,as.list(test1)))
})

test_that("the output from doCox is the same as the output from coxph",{
dCfit <- doCox(formula, data, firth=FALSE)$model
cfit <- survival::coxph(formula, data)
expect_equal(dCfit[-length(dCfit)],cfit[-length(cfit)])
})

test_that("the output from doCox is the same as the output from coxphf",{
dCfitf <- doCox(formula, data)$model
cfitf <- coxphf::coxphf(formula, data)
expect_equal(dCfitf,cfitf)
})

