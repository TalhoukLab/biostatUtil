
context("Cox model: generic and multivariable")

library(survival)
data(lung)

test_that("doCoxphGeneric HRs are different when reference group changes", {
  res1 <- doCoxphGeneric(input.d = lung,
                         var.names = "sex",
                         var.descriptions = "Sex",
                         show.var.detail = TRUE,
                         var.names.surv.time = "time",
                         var.names.surv.status = "status",
                         event.codes.surv = "2",
                         surv.descriptions = "OS",
                         caption = "")
  res2 <- doCoxphGeneric(input.d = lung,
                         var.names = "sex",
                         var.descriptions = "Sex",
                         show.var.detail = TRUE,
                         var.names.surv.time = "time",
                         var.names.surv.status = "status",
                         event.codes.surv = "2",
                         surv.descriptions = "OS",
                         caption = "",
                         var.ref.groups = "2",
                         round.small = TRUE)
  expect_length(res1, 3)
  expect_length(res2, 3)
  expect_identical(res1$result.table[, c(1, 3)], res2$result.table[, c(1, 3)])
  expect_false(res1$result.table[, c(2)] == res2$result.table[, c(2)])
})

test_that("doCoxphGeneric can fit multiple univariable models", {
  res <- doCoxphGeneric(input.d = lung,
                        var.names = c("sex", "ph.ecog"),
                        var.descriptions = c("Sex", "ECOG score"),
                        show.var.detail = TRUE,
                        var.names.surv.time = "time",
                        var.names.surv.status = "status",
                        event.codes.surv = "2",
                        surv.descriptions = "OS",
                        caption = "",
                        var.ref.groups = c("2", "0"))
  expect_equal(nrow(res$result.table), 2)
})

test_that("rounding of small p-values works", {
  res <- doCoxphGeneric(input.d = lung,
                        var.names = c("sex", "ph.ecog"),
                        var.descriptions = c("Sex", "ECOG score"),
                        show.var.detail = TRUE,
                        var.names.surv.time = "time",
                        var.names.surv.status = "status",
                        event.codes.surv = "2",
                        surv.descriptions = "OS",
                        caption = "",
                        round.small = TRUE)
  expect_equal(nrow(res$result.table), 2)
})

test_that("factor covariates use lowest group if no reference specified", {
  lung2 <- lung
  lung2$sex <- as.factor(lung2$sex)
  res1 <- doCoxphGeneric(input.d = lung,
                         var.names = "sex",
                         var.descriptions = "Sex",
                         show.var.detail = TRUE,
                         var.names.surv.time = "time",
                         var.names.surv.status = "status",
                         event.codes.surv = "2",
                         surv.descriptions = "OS",
                         caption = "")
  res2 <- doCoxphGeneric(input.d = lung2,
                         var.names = "sex",
                         var.descriptions = "Sex",
                         show.var.detail = TRUE,
                         var.names.surv.time = "time",
                         var.names.surv.status = "status",
                         event.codes.surv = "2",
                         surv.descriptions = "OS",
                         caption = "")
  expect_identical(res1$result.table, res2$result.table)
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

test_that("doInteractionCox works", {
  res <- doInteractionCox(input.d = lung,
                          var.names = "sex",
                          var.descriptions = "Sex",
                          var.names.surv.time = "time",
                          var.names.surv.status = "status",
                          event.codes.surv = "2",
                          surv.descriptions = "OS",
                          caption = "")
  expect_length(res, 4)
})