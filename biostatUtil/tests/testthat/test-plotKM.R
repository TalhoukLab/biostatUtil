
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

test_that("unused factor levels are removed", {
  lung$test <- factor(1:2, levels = 1:3)
  expect_error(doKMPlots(lung, "time", "status", "test", "Test", use.ggkm = FALSE), NA)
})

test_that("other plotting options can be specified", {
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = FALSE,
                         grey.scale = TRUE, shading.colors = NULL,
                         legend.pos = "top"), NA)
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = TRUE,
                         cox.ref.group = "2", show.risk = FALSE,
                         use.firth = 0.8), NA)
})

test_that("plot can be saved to file", {
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = FALSE,
                         file.name = "test.pdf"), NA)
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = FALSE,
                         file.name = "test.odf"))
  file.remove(list.files(pattern = "test\\."))
})

test_that("plot statistics indicated by reference group", {
  expect_error(doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = FALSE,
                         km.plot.ref.group = "2"), NA)
})

test_that("survival fits can be compared", {
  p1 <- doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = TRUE,
                  use.firth = 0.8, cox.ref.group = "2")
  p2 <- doKMPlots(lung, "time", "status", "sex", "Sex", use.ggkm = TRUE,
                  use.firth = 0.8, cox.ref.group = "2",
                  sfit2 = survfit(Surv(time, status) ~ age, lung))
  expect_false(identical(p1, p2))
})