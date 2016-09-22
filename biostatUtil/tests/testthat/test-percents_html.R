
context("HTML output of Row and Column percentages")

library(htmlTable)
set.seed(13)
B <- matrix(rbinom(20, size = 20, prob = 0.3), nrow = 4,
            dimnames = list(paste0("Row", 1:5), paste0("Col", 1:4)))

test_that("HTML output works", {
  htmlTable(colPercentAsHTML(B, keep = TRUE))
  htmlTable(colPercentAsHTML(B, keep = FALSE))
  htmlTable(rowPercentAsHTML(B, keep = TRUE))
  htmlTable(rowPercentAsHTML(B, keep = FALSE))
  htmlTable(rowColPercentAsHTML(B, keep = TRUE))
  htmlTable(rowColPercentAsHTML(B, keep = FALSE))
})

test_that("Banded rows alternate per row group", {
  htmlTable(colPercentAsHTML(B, keep = TRUE, banded.rows = TRUE))
  htmlTable(colPercentAsHTML(B, keep = FALSE, banded.rows = TRUE))
  htmlTable(rowPercentAsHTML(B, keep = TRUE, banded.rows = TRUE))
  htmlTable(rowPercentAsHTML(B, keep = FALSE, banded.rows = TRUE))
  htmlTable(rowColPercentAsHTML(B, keep = TRUE, banded.rows = TRUE))
  htmlTable(rowColPercentAsHTML(B, keep = FALSE, banded.rows = TRUE))
})

test_that("Other formatting options work", {
  htmlTable(rowPercentAsHTML(B, caption = "Test Caption"))
  htmlTable(colPercentAsHTML(B, html.table.border = 10))
  htmlTable(rowColPercentAsHTML(B, keep = FALSE, banded.rows = TRUE,
                                col.odd = "yellow", col.even = "green"))
})

test_that("Row and column names can be overwritten", {
  htmlTable(colPercentAsHTML(B, keep = TRUE, row.names = letters[1:4],
                             column.names = LETTERS[1:4]))
  htmlTable(rowPercentAsHTML(B, keep = TRUE, row.names = letters[1:4],
                             column.names = LETTERS[1:4]))
  htmlTable(rowColPercentAsHTML(B, keep = TRUE, row.names = letters[1:4],
                                column.names = LETTERS[1:4]))

})

test_that("Transpose can be useful", {
  set.seed(13)
  C <- matrix(rbinom(10, 10, 0.4), nrow = 1)
  D <- matrix(rbinom(10, 10, 0.4), ncol = 1)
  htmlTable(rowPercentAsHTML(C, transpose = FALSE))
  htmlTable(rowPercentAsHTML(C, transpose = TRUE))
  htmlTable(colPercentAsHTML(D, transpose = TRUE))
})