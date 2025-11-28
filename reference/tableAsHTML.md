# Table as HTML format

Generate HTML code for a table output. Assumes table has more than one
row.

## Usage

``` r
tableAsHTML(
  t,
  row.names = NULL,
  column.names = NULL,
  html.table.border = 0,
  banded.rows = FALSE,
  col.odd = "none",
  col.even = "lightgrey",
  caption = NA
)
```

## Arguments

- t:

  a matrix

- row.names:

  (optional) vector of row names in table

- column.names:

  (optional) vector of column names in table

- html.table.border:

  border type for the table. Defaults to 0 in HTML syntax.

- banded.rows:

  logical; if `TRUE`, alternating rows will have different shaded
  colours.

- col.odd:

  colour to use for odd numbered rows

- col.even:

  colour to use for even numbered rows

- caption:

  table caption. Uses
  [`addTableNumber()`](https://talhouklab.github.io/biostatUtil/reference/addTableNumber.md)
  to increment table number

## Value

A character string that can be parsed as HTML code to produce a nicer
table output.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
library(htmlTable)
A <- matrix(c(2, 3, 5, 10), nrow = 2, dimnames = list(c("Row1", "Row2"), c("Col1", "Col2")))
htmlTable(tableAsHTML(A, banded.rows = TRUE))
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;'  id='table_7'>
#> <tbody>
#> <tr style='border-top: 2px solid grey;'>
#> <td style='border-top: 2px solid grey; border-bottom: 2px solid grey; text-align: center;'><table border="0"><caption style="display: table-caption; text-align: left;"></caption><tr><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;"></th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Col1</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Col2</th></tr><tr style="background-color: none"><th>Row1</th><td>2</td><td>5</td></tr><tr style="background-color: lightgrey"><th>Row2</th><td>3</td><td>10</td></tr></table></td>
#> </tr>
#> </tbody>
#> </table>
```
