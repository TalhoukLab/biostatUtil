# Clean dates

Clean a date and reformat it to another style.

## Usage

``` r
cleanDate(
  x,
  original.format,
  preferred.format,
  existing.missing.codes = "",
  return.missing.code = NA,
  sep = "/"
)
```

## Arguments

- x:

  a date string or a numeric representation of a date (e.g. January
  13th, 1991 would be 19910113)

- original.format:

  format of input `x`

- preferred.format:

  format to change `x` to

- existing.missing.codes:

  missing dates

- return.missing.code:

  what to return if there is a missing input

- sep:

  date separator. Defaults to "/"

## Value

A date string cleaned and formatted from the original (unformatted) date

## See also

Other date formatting functions:
[`chr_to_date()`](https://talhouklab.github.io/biostatUtil/reference/chr_to_date.md),
[`formatDate()`](https://talhouklab.github.io/biostatUtil/reference/formatDate.md),
[`getFormat()`](https://talhouklab.github.io/biostatUtil/reference/getFormat.md),
[`numericToDate()`](https://talhouklab.github.io/biostatUtil/reference/numericToDate.md)

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
cleanDate("09/11/1991", original.format = "MM.DD.YYYY", preferred.format = "DD.MM.YYYY")
#> [1] "11/09/1991"
cleanDate(11091991, original.format = "DD.MM.YYYY", preferred.format = "YYYY.MMM.DD")
#> [1] "1991/Sep/11"
cleanDate(11091991, original.format = "DD.MM.YYYY", preferred.format = "YYYY.MMM.DD", sep = "-")
#> [1] "1991-Sep-11"
```
