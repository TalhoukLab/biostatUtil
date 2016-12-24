
context("Multiclass confusion matrices")

set.seed(23)
k <- 3
(x <- sample(1:k, 100, replace = TRUE, prob = c(0.15, 0.25, 0.6)))
(y <- sample(1:k, 100, replace = TRUE, prob = c(0.05, 0.4, 0.65)))
prop.table(table(y))

test_that("Same results obtained whether fed a character, a numeric or a factor", {
  expect_equal(
    multiClassCM(as.character(x), as.character(y))$table,
    multiClassCM(as.factor(x), as.factor(y))$table
  )
  expect_equal(
    multiClassCM(as.numeric(x), as.numeric(y))$table,
    multiClassCM(as.factor(x), as.factor(y))$table
  )
  expect_equal(
    multiClassCM(as.character(x), as.character(y))$table,
    multiClassCM(as.factor(x), as.factor(y))$table)
})

<<<<<<< HEAD
test_that("Results are equivalent to Caret",{
  (p <- sample(1:2, 100, replace=TRUE, prob=c(0.15, 0.25)))
  (q <- sample(1:2, 100, replace=TRUE, prob=c(0.05, 0.4)))
  a <- multiClassCM(p,q)
  b <- caret::confusionMatrix(p,q)
  })
=======
test_that("Error generated if uniques of x and y are not equivalent", {
  z <- sample(letters[1:k], 100, replace = TRUE, prob = c(0.05, 0.4, 0.65))
  expect_error(multiClassCM(y, z))
})
>>>>>>> 4827890f8af9d33af02f75ea3615ecc048f051e3
