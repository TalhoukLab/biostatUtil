# Latest and Earliest Dates

Finds the latest (max) and earliest (min) date given a vector of dates.
The functions ending in `Array` find the extremes for an array of dates
as opposed to a vector.

## Usage

``` r
maxDate(dates, na.rm = TRUE)

minDate(dates, na.rm = TRUE)

maxDateArray(
  t.arr,
  date.format = "MM.DD.YYYY",
  existing.missing.codes = NA,
  return.missing.code = NA,
  sep = "/"
)

minDateArray(
  t.arr,
  date.format = "MM.DD.YYYY",
  existing.missing.codes = NA,
  return.missing.code = NA,
  sep = "/"
)
```

## Arguments

- dates:

  vector of dates

- na.rm:

  logical. Should NAs be removed?

- t.arr:

  array of date strings

- date.format:

  format of the array of dates

- existing.missing.codes:

  missing dates

- return.missing.code:

  what to return if there is a missing input

- sep:

  date separator. Defaults to "/"

## Value

The latest or earliest date from a vector of dates.

## Details

The input vector should have dates formatted as YYYY-MM-DD. If `na.rm`
is not set to the default (`TRUE`) and `dates` has `NA` values, then the
function will also return `NA`.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
## No NA
t1 <- as.Date(c("2015-03-01", "2015-02-15", "2015-05-01"))
minDate(t1)
#> [1] "2015-02-15"
maxDate(t1)
#> [1] "2015-05-01"

## With NA
t2 <- as.Date(c("2015-03-01", "2015-02-15", NA, "2014-05-01"))
maxDate(t2)
#> [1] "2015-03-01"
minDate(t2)
#> [1] "2014-05-01"
minDate(t2, na.rm = FALSE)
#> [1] NA

## Array of dates
many.dates <- c("03/21/1992", "04/21/2013", "10/10/2015")
maxDateArray(many.dates)
#> [1] "10/10/2015"
minDateArray(many.dates)
#> [1] "03/21/1992"

many.dates <- c("2009-03-01", "2010-01-12", "2015-01-11")
maxDateArray(many.dates, sep = "-")
#> [1] "2015-01-11"
minDateArray(many.dates, sep = "-")
#> [1] "2009-03-01"

ties.dates <- c("2009-03-01", "2010-01-12", "2010-01-12")
maxDateArray(ties.dates, sep = "-")
#> [1] "2010-01-12"
minDateArray(ties.dates, sep = "-")
#> [1] "2009-03-01"
```
