
context("Principal component scatterplots and multiple plotting")

library(ggplot2)

test_that("PCAplot works", {
  expect_error(PCAplot(mtcars$cyl, mtcars), NA)
})

test_that("One plot can still use multiplot", {
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  expect_error(multiplot(p), NA)
})