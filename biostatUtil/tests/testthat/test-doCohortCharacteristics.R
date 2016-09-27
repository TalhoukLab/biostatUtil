
context("Compute cohort characteristics")

test_that("function returns four different results", {
  dcc <- doCohortCharacteristics(
    input.d = mtcars,
    marker.name = "cyl",
    marker.description = "cylinders",
    var.names = c("disp", "hp"),
    var.descriptions = c("displacement", "horsepower"),
    is.var.continuous = c(TRUE, TRUE),
    do.droplevels = TRUE,
    caption = "Some mtcars summaries") 
  expect_length(dcc, 4)
  expect_is(dcc$result.table, "matrix")
  expect_is(dcc$result.table.html, "character")
  expect_is(dcc$result.table.bamboo, "character")
})