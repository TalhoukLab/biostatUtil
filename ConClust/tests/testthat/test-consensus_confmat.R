
context("Consensus confusion matrix")

test_that("confusion matrix works as expected", {
  set.seed(2)
  a <- sample(1:4, 100, replace = TRUE)
  b <- sample(1:4, 100, replace = TRUE)
  expect_error(consensus_confmat(a, b, pred.lab = "Second Run",
                                 ref.lab = "First Run"), NA)
})
