# Row and Column Percentages

Calculate percentages in a table. `rowPercent` gives row percentages,
`colPercent` gives column percentages, and `rowColPercent` gives both
row and column percentages.

## Usage

``` r
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
```

## Arguments

- t:

  a matrix

- pretty.text:

  logical; if `TRUE`, will format the table into nice display

- keep:

  logical; if `TRUE`, the original table counts will be kept along with
  the percentages

- digits:

  number of digits to round to

- ...:

  additional arguments from `colPercent` and `rowPercent` for
  `rowColPercent`, or additional arguments from non-HTML functions to
  HTML functions.

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

- transpose:

  logical; if `TRUE`, the table is transposed.

## Value

A table with row-wise/column-wise percentages added. The percentages sum
to 1 per row/column.

HTML-based functions return the code used to generate a table that shows
row and/or column percentages.

## Details

Generates a table of row and/or column percentages given table `t`.
Using `pretty.text = TRUE` will add the \\

Row and column names can be replaced by `row.names` and `column.names`.
Higher values of `html.table.border` make the table borders thicker and
even look 3D. `col.odd` and `col.even` are ignored when
`banded.rows = FALSE`.

Transposing is useful if there are many columns in a single row or vice
versa. Note that the percentages are unchanged, only the orientation of
the table.

## Author

Aline Talhouk, Samuel Leung, Derek Chiu

## Examples

``` r
# Base outputs
A <- matrix(c(2, 3, 5, 10), nrow = 2, dimnames = list(c("Row1", "Row2"), c("Col1", "Col2")))
rowPercent(A)
#>              Col1    Col2
#> Row1       2.0000  5.0000
#> Row1 Row % 0.2857  0.7143
#> Row2       3.0000 10.0000
#> Row2 Row % 0.2308  0.7692
rowPercent(A, keep = FALSE)
#>        Col1   Col2
#> Row1 0.2857 0.7143
#> Row2 0.2308 0.7692
colPercent(A, pretty.text = TRUE)
#>            Col1  Col2    
#> Row1       "2"   "5"     
#> Row1 Col % "40%" "33.33%"
#> Row2       "3"   "10"    
#> Row2 Col % "60%" "66.67%"
colPercent(A, pretty.text = TRUE, keep = FALSE)
#>      Col1  Col2    
#> Row1 "40%" "33.33%"
#> Row2 "60%" "66.67%"
rowColPercent(A, digits = 2)
#>            Col1  Col2
#> Row1       2.00  5.00
#> Row1 Row % 0.29  0.71
#> Row1 Col % 0.40  0.33
#> Row2       3.00 10.00
#> Row2 Row % 0.23  0.77
#> Row2 Col % 0.60  0.67

# HTML outputs
library(htmlTable)
set.seed(13)
B <- matrix(rbinom(16, size = 20, prob = 0.3), nrow = 4,
dimnames = list(paste0("Row", 1:4), paste0("Col", 1:4)))
htmlTable(rowColPercentAsHTML(B, keep = TRUE, digits = 2, pretty.text = TRUE,
banded.rows = TRUE, col.odd = "yellow", col.even = "green", caption =
"Example Table", html.table.border = 2))
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;'  id='table_5'>
#> <tbody>
#> <tr style='border-top: 2px solid grey;'>
#> <td style='border-top: 2px solid grey; border-bottom: 2px solid grey; text-align: center;'><table border="2"><caption style="display: table-caption; text-align: left;">Table 4: Example Table</caption><tr><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;" colspan="2"></th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Col1</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Col2</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Col3</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Col4</th></tr><tr style="background-color: yellow"><th style="text-align: center; padding-right:10px; padding-right:10px;" rowspan="3">Row1</th><th style="text-align: center; padding-right:10px; padding-right:10px;">count</th><td>7</td><td>10</td><td>8</td><td>9</td></tr><tr style="background-color: yellow"><th style="text-align: center; padding-right:10px; padding-right:10px;">
#>   <i>row %</i>
#> </th><td>
#>   <i>21%</i>
#> </td><td>
#>   <i>29%</i>
#> </td><td>
#>   <i>24%</i>
#> </td><td>
#>   <i>26%</i>
#> </td></tr><tr style="background-color: yellow"><th style="text-align: center; padding-right:10px; padding-right:10px;">
#>   <i>col %</i>
#> </th><td>
#>   <i>35%</i>
#> </td><td>
#>   <i>40%</i>
#> </td><td>
#>   <i>31%</i>
#> </td><td>
#>   <i>35%</i>
#> </td></tr><tr style="background-color: green"><th style="text-align: center; padding-right:10px; padding-right:10px;" rowspan="3">Row2</th><th style="text-align: center; padding-right:10px; padding-right:10px;">count</th><td>5</td><td>2</td><td>3</td><td>6</td></tr><tr style="background-color: green"><th style="text-align: center; padding-right:10px; padding-right:10px;">
#>   <i>row %</i>
#> </th><td>
#>   <i>31%</i>
#> </td><td>
#>   <i>12%</i>
#> </td><td>
#>   <i>19%</i>
#> </td><td>
#>   <i>38%</i>
#> </td></tr><tr style="background-color: green"><th style="text-align: center; padding-right:10px; padding-right:10px;">
#>   <i>col %</i>
#> </th><td>
#>   <i>25%</i>
#> </td><td>
#>   <i>8%</i>
#> </td><td>
#>   <i>12%</i>
#> </td><td>
#>   <i>23%</i>
#> </td></tr><tr style="background-color: yellow"><th style="text-align: center; padding-right:10px; padding-right:10px;" rowspan="3">Row3</th><th style="text-align: center; padding-right:10px; padding-right:10px;">count</th><td>5</td><td>6</td><td>7</td><td>6</td></tr><tr style="background-color: yellow"><th style="text-align: center; padding-right:10px; padding-right:10px;">
#>   <i>row %</i>
#> </th><td>
#>   <i>21%</i>
#> </td><td>
#>   <i>25%</i>
#> </td><td>
#>   <i>29%</i>
#> </td><td>
#>   <i>25%</i>
#> </td></tr><tr style="background-color: yellow"><th style="text-align: center; padding-right:10px; padding-right:10px;">
#>   <i>col %</i>
#> </th><td>
#>   <i>25%</i>
#> </td><td>
#>   <i>24%</i>
#> </td><td>
#>   <i>27%</i>
#> </td><td>
#>   <i>23%</i>
#> </td></tr><tr style="background-color: green"><th style="text-align: center; padding-right:10px; padding-right:10px;" rowspan="3">Row4</th><th style="text-align: center; padding-right:10px; padding-right:10px;">count</th><td>3</td><td>7</td><td>8</td><td>5</td></tr><tr style="background-color: green"><th style="text-align: center; padding-right:10px; padding-right:10px;">
#>   <i>row %</i>
#> </th><td>
#>   <i>13%</i>
#> </td><td>
#>   <i>30%</i>
#> </td><td>
#>   <i>35%</i>
#> </td><td>
#>   <i>22%</i>
#> </td></tr><tr style="background-color: green"><th style="text-align: center; padding-right:10px; padding-right:10px;">
#>   <i>col %</i>
#> </th><td>
#>   <i>15%</i>
#> </td><td>
#>   <i>28%</i>
#> </td><td>
#>   <i>31%</i>
#> </td><td>
#>   <i>19%</i>
#> </td></tr></table></td>
#> </tr>
#> </tbody>
#> </table>
```
