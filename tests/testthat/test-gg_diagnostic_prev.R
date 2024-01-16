test_that("gg_diagnostic_prev is outputted", {
  se <- c(0.5, 0.7, 0.9)
  sp <- seq(0.21, 1, 0.01)
  p <- c(0.32, 0.09, 0.026)

  pp1 <- gg_diagnostic_prev(se, sp, p, result = "PPV")
  pp2 <- gg_diagnostic_prev(se, sp, p, result = "NPV")

  expect_error(pp1, NA)
  expect_s3_class(pp1, "ggplot")

  expect_error(pp2, NA)
  expect_s3_class(pp2, "ggplot")
})

test_that("gg_prev_fixed is outputted", {
  se <- c(0.67, 0.83, 0.97)
  sp <- c(0.86, 0.94, 0.99)
  p <- seq(0.01, 0.30, 0.01)

  pp3 <- gg_prev_fixed(se, sp, p, result = "PPV")
  pp4 <- gg_prev_fixed(se, sp, p, result = "NPV")

  expect_error(pp3, NA)
  expect_s3_class(pp3, "ggplot")

  expect_error(pp4, NA)
  expect_s3_class(pp4, "ggplot")
})
