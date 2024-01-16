data(cancer, package = "survival")
set.seed(1)
lung$time2 <- sample(lung$time, replace = TRUE)
lung$status2 <- sample(lung$status, replace = TRUE)
res <- doCoxphGeneric(input.d = lung,
                      var.names = "sex",
                      var.descriptions = "Sex",
                      show.var.detail = TRUE,
                      var.names.surv.time = "time",
                      var.names.surv.status = "status",
                      event.codes.surv = "2",
                      surv.descriptions = "OS",
                      caption = "")

test_that("extract_cox is correct", {
  expect_equal(extract_cox(res, "sex", "OS"),
               "HR 0.59 (0.42-0.82), p = 0.0015")
})
