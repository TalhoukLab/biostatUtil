
context("Plot KM")

library(survival)
data(lung)

test_that("doKMPlots calls ggkm or plotKM", {
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = FALSE), NA)
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = TRUE), NA)
})

test_that("factor variable drops unused levels", {
  lung$sex <- as.factor(lung$sex)
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex"), NA)
})

test_that("other plotting options can be specified", {
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = FALSE,
                         grey.scale = TRUE, shading.colors = NULL,
                         legend.pos = "top"), NA)
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = TRUE,
                         show.risk = FALSE), NA)
})

test_that("unused factor levels are removed", {
  lung$test <- factor(1:2, levels = 1:3)
  expect_error(doKMPlots(lung, "time", "status", "test", "Test", use.ggkm = FALSE), NA)
})