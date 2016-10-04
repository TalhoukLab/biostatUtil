
context("Assess survival time")

set.seed(3)
starts <- seq(as.Date("2000/1/1"), as.Date("2003/1/1"), by = "quarter")
ends <-  seq(as.Date("2003/1/1"), as.Date("2006/1/1"), by = "quarter")
statuses <- sample(0:1, 13, replace = TRUE)

test_that("output is a list of 5 real-valued elements", {
  x1 <- assessSurvTime(starts, ends, statuses)
  expect_is(x, "list")
  expect_length(x, 5)
})

test_that("missing cases are handled", {
  statuses[9:12] <- NA
  x2 <- assessSurvTime(starts, ends, statuses)
  expect_false(isTRUE(all.equal(x1, x2)))
})
