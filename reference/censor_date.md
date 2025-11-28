# Date of censoring

Calculate date of censoring using date of diagnosis and last follow-up.

## Usage

``` r
censor_date(date_dx, date_lfu, cutoff_yr)
```

## Arguments

- date_dx:

  date of diagnosis

- date_lfu:

  date of last follow-up

- cutoff_yr:

  number of years to cutoff at

## Value

date of censoring

## Details

The cutoff date is `cutoff_yr` years after the date of diagnosis, on
December 31st. If the cutoff date is earlier than the date of last
follow-up, then the cutoff date is the new date of censoring, otherwise
use the date of last follow-up.

## Author

Derek Chiu

## Examples

``` r
date_dx <- as.Date("2003-03-08")
date_lfu <- as.Date("2009-08-09")
censor_date(date_dx, date_lfu, 5)
#> [1] "2008-12-31"
censor_date(date_dx, date_lfu, 6)
#> [1] "2009-08-09"
```
