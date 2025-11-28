# Change character to date

Change a numeric value stored as a character string to a date object.

## Usage

``` r
chr_to_date(x, date.origin = EXCEL.ORIGIN)
```

## Arguments

- x:

  a number that represents the number of days after `date.origin`

- date.origin:

  the date from which we count the number of days passed

## Value

A date object, converted from a character string.

## Details

The default `date.origin` used is the Excel 1900 version.

## See also

Other date formatting functions:
[`cleanDate()`](https://talhouklab.github.io/biostatUtil/reference/cleanDate.md),
[`formatDate()`](https://talhouklab.github.io/biostatUtil/reference/formatDate.md),
[`getFormat()`](https://talhouklab.github.io/biostatUtil/reference/getFormat.md),
[`numericToDate()`](https://talhouklab.github.io/biostatUtil/reference/numericToDate.md)

## Author

Derek Chiu

## Examples

``` r
chr_to_date("41041")
#> [1] "2012-05-12"
```
