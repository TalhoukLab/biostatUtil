
context("Classification accuracy")

test_that("accuracy works", {
  set.seed(1)
  x <- matrix(rbinom(16, 20, 0.4), nrow = 4)
  expect_equal(sum(diag(x)) / sum(x), accuracy(x))
})
