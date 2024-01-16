set.seed(1)
x <- rexp(100, rate = 0.5)
y <- rexp(100, rate = 0.3)
y[3] <- NA
z <- rep(NA, 100)

test_that("Geometric mean works", {
  expect_type(geoMean(x), "double")
  expect_type(geoMean(x, na.rm = TRUE), "double")
})

test_that("Missing elements are handled", {
  expect_type(geoMean(y, na.rm = TRUE), "double")
  expect_equal(NA_real_, geoMean(y))
  expect_equal(NA, geoMean(z, na.rm = TRUE))
})
