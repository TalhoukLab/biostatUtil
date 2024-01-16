test_that("design_matrix has correct number of columns", {
  set.seed(2021)
  n <- 4
  df <- data.frame(matrix(rnorm(40), ncol = n))
  dm2 <- design_matrix(df, order = 2)
  expect_equal(ncol(dm2), sum(choose(n, 0:2)))

  dm3 <- design_matrix(df, order = 3)
  expect_equal(ncol(dm3), sum(choose(n, 0:3)))
})
