# Do histogram with median

Plot a histogram with the median, first quartile, and third quartile
reported.

## Usage

``` r
doHist(
  data,
  var,
  xlab = var,
  title = NULL,
  show.title = TRUE,
  br = ceiling(sqrt(nrow(data))),
  digits = 3,
  score.lab = "scorable",
  ...
)
```

## Arguments

- data:

  data.frame with column names

- var:

  string of variable in `data` to graph on

- xlab:

  x-axis label

- title:

  histogram title

- show.title:

  logical. If `TRUE` (default), the title is shown

- br:

  breaks in histogram. By default, the number of bins is taking as the
  ceiling of the square root of the number of rows in `data`

- digits:

  number of digits to round for median, Q1, Q3

- score.lab:

  label for non-missing cases

- ...:

  additional arguments to `hist`

## Value

A histogram with some annotated values.

## Details

Expects missing to be `NA`. Do not filter out missing data as this
function reports missing data counts.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
doHist(mtcars, "mpg")

doHist(mtcars, "mpg", "MPG")

doHist(mtcars, "mpg", title = "Distribution of MPG")
```
