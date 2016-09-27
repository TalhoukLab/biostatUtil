
context("Consensus cluster plus")

set.seed(23)
x1 <- ConClustPlus(matrix(rnorm(100), nrow = 10), k = 3, reps = 10, pItem = 0.9)
set.seed(23)
x2 <- ConClustPlus(matrix(rnorm(100), nrow = 10), k = 3, reps = 10, pItem = 0.9,
                   save = TRUE)

test_that("output has correct structure", {
  expect_length(x, 6)
  expect_true(all(sapply(x, length) == 3))
})

test_that("output can be saved", {
  expect_identical(x1, x2)
  file.remove(list.files(pattern = "results_CCP"))
})
