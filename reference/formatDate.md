# Format a date.

Prints a date into a pretty format.

## Usage

``` r
formatDate(
  d,
  m,
  y,
  date.format = c("MM.DD.YYYY", "MMM.DD.YYYY", "DD.MM.YYYY", "DD.MMM.YYYY", "YYYY.MM.DD",
    "YYYY.MMM.DD"),
  sep = "/"
)
```

## Arguments

- d:

  day of the month (1-31)

- m:

  month of the year (1-12)

- y:

  year of date

- date.format:

  how to format the date. Defaults to "month/day/year".

- sep:

  string used to separate `d`, `m`, and `y`. Defaults to "/".

## Value

A character string of a formatted date.

## Details

Given the day, month, and year of a date, returns the date in a specific
format. The order and separating string can be modified using
`date.format` and `sep` respectively. Take note the order of the
arguments: day, month, and year. Only accepts "MM.DD.YYYY",
"MMM.DD.YYYY", "DD.MM.YYYY", "DD.MMM.YYYY", "YYYY.MM.DD", "YYYY.MMM.DD".

## See also

Other date formatting functions:
[`chr_to_date()`](https://talhouklab.github.io/biostatUtil/reference/chr_to_date.md),
[`cleanDate()`](https://talhouklab.github.io/biostatUtil/reference/cleanDate.md),
[`getFormat()`](https://talhouklab.github.io/biostatUtil/reference/getFormat.md),
[`numericToDate()`](https://talhouklab.github.io/biostatUtil/reference/numericToDate.md)

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
formatDate(8, 7, 2011)
#> [1] "07/08/2011"
formatDate(8, 7, 2011, date.format = "YYYY.MM.DD")
#> [1] "2011/07/08"
formatDate(8, 7, 2011, date.format = "DD.MM.YYYY", sep = "-")
#> [1] "08-07-2011"
formatDate(10, 1, 2015, date.format = "MMM.DD.YYYY", sep = "-")
#> [1] "Jan-10-2015"
```
