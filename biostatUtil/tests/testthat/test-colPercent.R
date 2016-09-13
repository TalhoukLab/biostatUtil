
context("Column percentages")

A <- matrix(c(2, 3, 5, 10), nrow = 2,
            dimnames = list(c("Row1", "Row2"), c("Col1", "Col2")))

test_that("default text and keeping original values work", {
  expect_error(colPercent(A), NA)
})

test_that("default text and dropping original values work", {
  expect_error(colPercent(A, keep = FALSE), NA)
})

test_that("pretty text and keeping original values work", {
  expect_error(colPercent(A, pretty.text = TRUE), NA)
})

test_that("pretty text and dropping original values work", {
  expect_error(colPercent(A, pretty.text = TRUE, keep = FALSE), NA)
})

test_that("table format returns different types", {
  expect_equal(typeof(colPercent(A)), "double")
  expect_equal(typeof(colPercent(A, pretty.text = TRUE)), "character")
})