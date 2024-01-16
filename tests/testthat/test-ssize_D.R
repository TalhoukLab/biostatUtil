library(survival)
fit <- coxph(Surv(time, status) ~ ph.ecog, data = lung)

test_that("sample size can be calculated for superiority trial", {
  expect_equal(ssize_D(fit, trial = "superiority"), 519)
})

test_that("sample size can be calculated for non-inferiority trial", {
  expect_equal(ssize_D(fit, trial = "non-inferiority"), 409)
})

test_that("only two trial types work", {
  expect_error(ssize_D(fit, trial = "inferiority"))
})
