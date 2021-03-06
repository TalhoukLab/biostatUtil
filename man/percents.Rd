% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percents.R, R/percents_html.R
\name{percents}
\alias{percents}
\alias{colPercent}
\alias{rowPercent}
\alias{rowColPercent}
\alias{colPercentAsHTML}
\alias{rowPercentAsHTML}
\alias{rowColPercentAsHTML}
\title{Row and Column Percentages}
\usage{
colPercent(t, pretty.text = FALSE, keep = TRUE, digits = 4)

rowPercent(t, pretty.text = FALSE, keep = TRUE, digits = 4)

rowColPercent(t, keep = TRUE, ...)

colPercentAsHTML(
  t,
  keep = TRUE,
  row.names = NULL,
  column.names = NULL,
  html.table.border = 0,
  banded.rows = FALSE,
  col.odd = "none",
  col.even = "lightgrey",
  caption = NA,
  transpose = FALSE,
  ...
)

rowPercentAsHTML(
  t,
  keep = TRUE,
  row.names = NULL,
  column.names = NULL,
  html.table.border = 0,
  banded.rows = FALSE,
  col.odd = "none",
  col.even = "lightgrey",
  caption = NA,
  transpose = FALSE,
  ...
)

rowColPercentAsHTML(
  t,
  keep = TRUE,
  row.names = NULL,
  column.names = NULL,
  html.table.border = 0,
  banded.rows = FALSE,
  col.odd = "none",
  col.even = "lightgrey",
  caption = NA,
  ...
)
}
\arguments{
\item{t}{a matrix}

\item{pretty.text}{logical; if \code{TRUE}, will format the table into nice
display}

\item{keep}{logical; if \code{TRUE}, the original table counts will be kept
along with the percentages}

\item{digits}{number of digits to round to}

\item{...}{additional arguments from \code{colPercent} and \code{rowPercent}
for \code{rowColPercent}, or additional arguments from non-HTML functions
to HTML functions.}

\item{row.names}{(optional) vector of row names in table}

\item{column.names}{(optional) vector of column names in table}

\item{html.table.border}{border type for the table. Defaults to 0 in HTML
syntax.}

\item{banded.rows}{logical; if \code{TRUE}, alternating rows will have
different shaded colours.}

\item{col.odd}{colour to use for odd numbered rows}

\item{col.even}{colour to use for even numbered rows}

\item{caption}{table caption. Uses \code{\link[=addTableNumber]{addTableNumber()}} to increment
table number}

\item{transpose}{logical; if \code{TRUE}, the table is transposed.}
}
\value{
A table with row-wise/column-wise percentages added. The percentages
sum to 1 per row/column.

HTML-based functions return the code used to generate a table that
shows row and/or column percentages.
}
\description{
Calculate percentages in a table. \code{rowPercent} gives row percentages,
\code{colPercent} gives column percentages, and \code{rowColPercent} gives
both row and column percentages.
}
\details{
Generates a table of row and/or column percentages given table \code{t}.
Using \code{pretty.text = TRUE} will add the \\% sign to the percentages.

Row and column names can be replaced by \code{row.names} and
\code{column.names}. Higher values of \code{html.table.border} make the table
borders thicker and even look 3D. \code{col.odd} and \code{col.even} are
ignored when \code{banded.rows = FALSE}.

Transposing is useful if there are many columns in a single row or vice
versa. Note that the percentages are unchanged, only the orientation of the
table.
}
\examples{
# Base outputs
A <- matrix(c(2, 3, 5, 10), nrow = 2, dimnames = list(c("Row1", "Row2"), c("Col1", "Col2")))
rowPercent(A)
rowPercent(A, keep = FALSE)
colPercent(A, pretty.text = TRUE)
colPercent(A, pretty.text = TRUE, keep = FALSE)
rowColPercent(A, digits = 2)

# HTML outputs
library(htmlTable)
set.seed(13)
B <- matrix(rbinom(16, size = 20, prob = 0.3), nrow = 4,
dimnames = list(paste0("Row", 1:4), paste0("Col", 1:4)))
htmlTable(rowColPercentAsHTML(B, keep = TRUE, digits = 2, pretty.text = TRUE,
banded.rows = TRUE, col.odd = "yellow", col.even = "green", caption =
"Example Table", html.table.border = 2))
}
\author{
Aline Talhouk, Samuel Leung, Derek Chiu
}
