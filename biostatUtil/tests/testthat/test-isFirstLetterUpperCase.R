
context("First letter uppercase")

test_that("first letter lowercase is FALSE", {
  expect_false(isFirstLetterUpperCase("fALSE"))
})

test_that("first letter uppercase is TRUE", {
  expect_true(isFirstLetterUpperCase("True"))
})

test_that("first character empty string returns TRUE", {
  expect_true(isFirstLetterUpperCase(""))
  expect_true(isFirstLetterUpperCase(" true"))
})