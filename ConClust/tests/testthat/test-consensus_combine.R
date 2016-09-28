
context("Consensus results combine")

set.seed(911)
x <- matrix(rnorm(1000), nrow = 10)
CC1 <- ConClust(x, k = 4, reps = 10, method = "apEucl", save = FALSE)
CC2 <- ConClust(x, k = 4, reps = 10, method = "gmmBIC", save = FALSE)
CCP <- ConClustPlus(x, k = 4, reps = 10, save = FALSE)

test_that("combining results has expected lengths", {
  y1 <- consensus_combine(CC1, CC2, res.CCP = CCP, k = 4,
                          element = "matrix")
  y2 <- consensus_combine(CC1, CC2, res.CCP = CCP, k = 4,
                          element = "class")
  expect_length(y1, length(CCP) + dim(CC1)[3] + dim(CC2)[3])
  expect_equal(ncol(y2), length(CCP) + dim(CC1)[3] + dim(CC2)[3])
})

test_that("names can be overwritten", {
  y3 <- consensus_combine(CC1, CC2, res.CCP = CCP, k = 4,
                          element = "matrix", alg.names = paste0("A", 1:8))
  y4 <- consensus_combine(CC1, CC2, res.CCP = CCP, k = 4,
                          element = "class", alg.names = paste0("A", 1:8))
  expect_identical(names(y3), paste0("A", 1:8))
  expect_identical(names(y4), paste0("A", 1:8))
})
