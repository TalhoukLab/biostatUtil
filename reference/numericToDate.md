# Change numeric to date

Change a numeric value to a date object by specifying a date of origin.

## Usage

``` r
numericToDate(x, date.origin = DATE.ORIGIN)
```

## Arguments

- x:

  a number that represents the number of days after `date.origin`

- date.origin:

  the date from which we count the number of days passed

## Value

A date object, converted from a numeric object.

## See also

Other date formatting functions:
[`chr_to_date()`](https://talhouklab.github.io/biostatUtil/reference/chr_to_date.md),
[`cleanDate()`](https://talhouklab.github.io/biostatUtil/reference/cleanDate.md),
[`formatDate()`](https://talhouklab.github.io/biostatUtil/reference/formatDate.md),
[`getFormat()`](https://talhouklab.github.io/biostatUtil/reference/getFormat.md)

## Author

Samuel Leung

## Examples

``` r
numericToDate(10)
#> [1] "1970-01-11"
numericToDate(10, "2000-09-11")
#> [1] "2000-09-21"
```
