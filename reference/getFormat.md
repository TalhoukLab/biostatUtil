# Get date format from character text

Get the POSIX standard date formats from character text formats.

## Usage

``` r
getFormat(
  date,
  char.format = c("MM.DD.YYYY", "MMM.DD.YYYY", "DD.MM.YYYY", "DD.MMM.YYYY", "YYYY.MM.DD",
    "YYYY.MMM.DD"),
  sep = ""
)
```

## Arguments

- date:

  character string of date

- char.format:

  character text format of date

- sep:

  character string separating `date`

## Value

A character string representing the POSIX standard date format
equivalent of the string in `char.format`.

## See also

Other date formatting functions:
[`chr_to_date()`](https://talhouklab.github.io/biostatUtil/reference/chr_to_date.md),
[`cleanDate()`](https://talhouklab.github.io/biostatUtil/reference/cleanDate.md),
[`formatDate()`](https://talhouklab.github.io/biostatUtil/reference/formatDate.md),
[`numericToDate()`](https://talhouklab.github.io/biostatUtil/reference/numericToDate.md)

## Author

Derek Chiu

## Examples

``` r
getFormat("12/09/1993", "MM.DD.YYYY")
#> [1] "%m/%d/%Y"
getFormat("2005-09-13", "YYYY.MM.DD")
#> [1] "%Y-%m-%d"
```
