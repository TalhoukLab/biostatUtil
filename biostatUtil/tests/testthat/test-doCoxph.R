
context("Cox model: generic and multivariable")

library(survival)
data(lung)

test_that("doCoxphGeneric works", {
  res <- doCoxphGeneric(input.d = lung,
                        var.names = "sex",
                        var.descriptions = "Sex",
                        var.names.surv.time = "time",
                        var.names.surv.status = "status",
                        event.codes.surv = "2",
                        surv.descriptions = "OS",
                        caption = "")
  expect_length(res, 3)
})

test_that("doCoxphMultivariable works for univariable case", {
  res <- doCoxphMultivariable(input.d = lung,
                              var.names = "sex",
                              var.descriptions = "Sex",
                              var.names.surv.time = "time",
                              var.names.surv.status = "status",
                              event.codes.surv = "2",
                              surv.descriptions = "OS",
                              caption = "")
  expect_length(res, 4)
})