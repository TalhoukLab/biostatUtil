context("Cox proportional hazard models")

library(survival)
set.seed(12)
n <- 200  # number of cases
test.d <- data.frame(
  os.yrs = sample(1:1000, size = n, replace = TRUE) / 10,
  os.sts = sample(
    c("os.censored", "os.event"),
    size = n,
    replace = TRUE
  ),
  age = sample(30:90, size = n, replace = TRUE),
  grade = sample(1:3, size = n, replace = TRUE),
  stringsAsFactors = FALSE
)

test_that("output mirrors survival::coxph when use.firth==1", {
  surv_formula_1 <- Surv(os.yrs, os.sts == "os.event") ~ age + grade
  fit_coxph <- coxph(surv_formula_1, data = test.d)
  fit_prettyCoxph <- prettyCoxph(surv_formula_1, test.d) # default not use firth
  expect_equal(fit_coxph$n, fit_prettyCoxph$n)
})

test1 <- list(time = c(4, 3, 1, 1, 2, 2, 3),
              status = c(1, 1, 1, 0, 1, 1, 0),
              x = c(0, 2, 1, 1, 1, 0, 0),
              sex = c(0, 0, 0, 0, 1, 1, 1))
test1$x <- factor(test1$x)

test_that("output mirrors survival::coxph when use.firth==FALSE", {
  fit_coxph <- coxph(Surv(time, status) ~ x + strata(sex), test1)
  expect_equal(
    fit_coxph$n,
    prettyCoxph(
      Surv(time, status) ~ x + sex,
      data.frame(test1),
      use.firth = FALSE,
      check.ph = TRUE
    )$n
  )
})

test_that("reference group can be redefined", {
  pc1 <- prettyCoxph(Surv(time, status) ~ x + strata(sex), test1)
  pc2 <- prettyCoxph(Surv(time, status) ~ x + strata(sex), test1, ref.grp =
                       setNames("2", "x"))
  expect_equal(names(pc1$fit$coefficients), c("x1", "x2"))
  expect_equal(names(pc2$fit$coefficients), c("x0", "x1"))
})

test_that("PH residual plot can be saved", {
  expect_error(prettyCoxph(Surv(time, status) ~ x + sex, test1,
                           ph.test.plot.filename = "PH.pdf", check.ph = TRUE),
               NA)
})

test_that("Firth's correction can be used", {
  expect_true(prettyCoxph(Surv(time, status) ~ x + strata(sex), test1,
                          use.firth = 0.2)$used.firth)
})

test_that("Univariable model has no formatting issues", {
  expect_error(prettyCoxph(Surv(time, status) ~ sex, test1), NA)
})

dev.off()
if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
if (file.exists("PH.pdf")) file.remove("PH.pdf")
