context("Multiclass confusion matrices")

set.seed(23)
(x <- sample(1:k, 100, replace=TRUE, prob=c(0.15, 0.25,0.6)))
(y <- sample(1:k, 100, replace=TRUE, prob=c(0.05, 0.4,0.65)))
prop.table(table(y))

expect_equal(multiClassCM(as.character(x),as.character(y))$table==multiClassCM(as.factor(x),as.factor(y))$table)

test_that(" Same results obtained whether fed a character, a numeric or a factor",{
expect_equal(multiClassCM(as.character(x),as.character(y))$table,multiClassCM(as.factor(x),as.factor(y))$table)
expect_equal(multiClassCM(as.numeric(x),as.numeric(y))$table,multiClassCM(as.factor(x),as.factor(y))$table)
})

test_that("Error generated if uniques of x and y are not equivalent",{
z <- sample(letters[1:k], 100, replace=TRUE, prob=c(0.05, 0.4,0.65))
expect_error(multiClassCM(y,z))
})

