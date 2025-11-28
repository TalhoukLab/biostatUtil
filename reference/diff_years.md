# Date difference in years

Calculate date difference in years, taking into account leap years.

## Usage

``` r
diff_years(start, stop)
```

## Arguments

- start:

  starting date of time interval

- stop:

  ending date of time interval

## Value

A numeric value representing the date difference in years

## Details

Since the number of days in a year changes depending on whether said
year is a leap year, dividing a period of time by estimates such as
365.25, 365.241, 365.24442 will be slightly biased. The correct way to
calculate a length of period in years is to take into account which
years are leap years and divide by 365 or 366 as needed.

## Examples

``` r
start <- as.Date("2009-03-08")
stop <- as.Date("2009-08-09")
diff_years(start, stop)
#> [1] 0.4219178
```
