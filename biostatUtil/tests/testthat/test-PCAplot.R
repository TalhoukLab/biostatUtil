
context("Principal component scatterplots")

test_that("PCAplot works", {
  expect_error(PCAplot(mtcars$cyl, mtcars), NA)
})