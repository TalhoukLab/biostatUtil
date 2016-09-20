
context("HTML output of Row and Column percentages")

library(htmlTable)
set.seed(13)
B <- matrix(rbinom(16, size = 20, prob = 0.3), nrow = 4,
            dimnames = list(paste0("Row", 1:4), paste0("Col", 1:4)))

test_that("HTML output works", {
  htmlTable(rowColPercentAsHTML(B, keep = TRUE))
  htmlTable(rowColPercentAsHTML(B, keep = FALSE))
})

test_that("Banded rows alternate per row group", {
  htmlTable(rowColPercentAsHTML(B, keep = TRUE, banded.rows = TRUE))
  htmlTable(rowColPercentAsHTML(B, keep = FALSE, banded.rows = TRUE))
  htmlTable(rowColPercentAsHTML(B, keep = FALSE, banded.rows = TRUE,
                                col.odd = "yellow", col.even = "green"))
})

test_that("Caption prints", {
  htmlTable(rowColPercentAsHTML(B, caption = "Test Caption"))
})

test_that("Row and column names can be overwritten", {
  htmlTable(rowColPercentAsHTML(B, keep = TRUE, row.names = letters[1:4],
                                column.names = LETTERS[1:4]))
})