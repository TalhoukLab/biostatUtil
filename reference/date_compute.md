# Date computations

`addToDate` adds a period of time to a date string and returns a new
date string. `diffDate` takes the difference between two dates and
returns in the specified unit of time. `compareDate` compares the
difference between two dates.

## Usage

``` r
addToDate(
  org.date,
  delta,
  date.format = "MM.DD.YYYY",
  units = c("days", "weeks", "months", "years"),
  existing.missing.codes = NA,
  return.missing.code = NA,
  sep = "/"
)

diffDate(
  d1,
  d2,
  date.format = "MM.DD.YYYY",
  units = c("days", "weeks", "months", "years"),
  existing.missing.codes = NA,
  return.missing.code = NA,
  sep = "/"
)

compareDate(
  d1,
  d2,
  date.format = "MM.DD.YYYY",
  existing.missing.codes = NA,
  return.missing.code = NA,
  sep = "/"
)
```

## Arguments

- org.date:

  original date

- delta:

  amount of time to add to `org.date`

- date.format:

  how to format the resulting date

- units:

  for `addToDate`, the unit of time for `delta`; for `diffDate`, the
  unit of time for which to take the difference. Defaults to "days".

- existing.missing.codes:

  missing dates

- return.missing.code:

  what to return if there is a missing input

- sep:

  date separator string

- d1:

  later date

- d2:

  earlier date

## Value

`addDate` returns the new date after adding `delta` to `org.date`.

`diffDate` returns the difference between two dates `d1 - d2` in the
specified unit of time.

`compareDate` returns 1 if `d1 > d2`, -1 if `d1 < d2`, and 0 if
`d1 == d2`.

## Details

If `delta` is negative, then the returned date will be earlier than
`org.date`. The output date format will be the same as the input date
format.

The unit of time to add to or return in can be in `"days"`, `"weeks"`,
`"months"`, or `"years"`. `compareDate` calls `diffDate` and returns
integer values specifying which date is earlier (or if they are the
same). `d1` should be later than `d2` so the function returns
nonnegative values.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
## Adding to a date
addToDate("2014/07/08", 10, date.format = "YYYY.MM.DD")
#> [1] "2014/07/18"
addToDate("2014-07-08", 10, date.format = "YYYY.MM.DD", sep = "-")
#> [1] "2014-07-18"
addToDate("2014/07/08", 10, date.format = "YYYY.MM.DD", units = "months")
#> [1] "2015/05/09"
addToDate("2014/07/08", -10, date.format = "YYYY.MM.DD", units = "years")
#> [1] "2004/07/07"

## Date differences
# Later date comes first, subtracts the second date
diffDate("2003/03/21", "1992/01/27", date.format = "YYYY.MM.DD")
#> [1] 4071

# Otherwise negative
diffDate("1992/01/27", "2003/03/21", date.format = "YYYY.MM.DD")
#> [1] -4071

# Different separator
diffDate("2003-03-21", "1992-01-27", date.format = "YYYY.MM.DD", sep = "-")
#> [1] 4071

## Date comparisons
compareDate("01/22/1949", "04/13/1950", date.format = "MM.DD.YYYY")
#> [1] -1
compareDate("04/13/1950", "04/13/1950", date.format = "MM.DD.YYYY")
#> [1] 0
compareDate("04/13/1959", "04/13/1950", date.format = "MM.DD.YYYY")
#> [1] 1
compareDate("01-22-1949", "04-13-1950", date.format = "MM.DD.YYYY", sep =
"-")
#> [1] -1
```
