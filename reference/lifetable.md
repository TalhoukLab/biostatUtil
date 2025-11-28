# Generate lifetables for multiclass variables

Specify vector of time endpoints and create a cohort life table for two
or more strata

## Usage

``` r
lifetable(
  obj,
  ntimes = 3,
  times = NULL,
  nround = 3,
  show.strata = TRUE,
  strata.name = "strata",
  summary = FALSE
)
```

## Arguments

- obj:

  An object of class `survfit`

- ntimes:

  number of time intervals

- times:

  A vector of endpoints of time intervals to show life table
  calculations. By default, these are `ntimes` evenly spaced out
  endpoints based on the full range of survival times.

- nround:

  number of digits to round table values

- show.strata:

  logical; if `TRUE` (default), the variable name is appended to the
  beginning of each stratum in the lifetable's `strata` column

- strata.name:

  column name for the different strata

- summary:

  logical; if `TRUE`, a case processing summary is shown with number of
  subjects, events, censored, and percent censored per stratum.

## Value

A table with the following columns:

- strata:

  name of specific group in variable

- times:

  time interval

- nsubs:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- nlost:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- nrisk:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- nevent:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- surv:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- pdf:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- hazard:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- se.surv:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- se.pdf:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

- se.hazard:

  See [`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

## Details

Essentially a wrapper around
[`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html) that
allows the user to input a `survfit` object instead of vectors of raw
values.

## See also

[`KMsurv::lifetab()`](https://rdrr.io/pkg/KMsurv/man/lifetab.html)

## Author

Derek Chiu

## Examples

``` r
library(survival)
obj <- survfit(Surv(futime, fustat) ~ rx, data = ovarian)
lifetable(obj)
#>   strata    times nsubs nlost nrisk nevent  surv   pdf hazard se.surv se.pdf
#> 1   rx=1    0-424    13     0  13.0      6 1.000 0.001  0.001   0.000      0
#> 2   rx=1  424-761     7     3   5.5      1 0.538 0.000  0.001   0.138      0
#> 3   rx=1 761-1227     3     3   1.5      0 0.441    NA     NA   0.144     NA
#> 4   rx=2    0-424    13     2  12.0      2 1.000 0.000  0.000   0.000      0
#> 5   rx=2  424-761     9     2   8.0      3 0.833 0.001  0.001   0.108      0
#> 6   rx=2 761-1227     4     4   2.0      0 0.521    NA     NA   0.158     NA
#>   se.hazard
#> 1     0.001
#> 2     0.001
#> 3        NA
#> 4     0.000
#> 5     0.001
#> 6        NA
lifetable(obj, ntimes = 4, show.strata = FALSE)
#>   strata    times nsubs nlost nrisk nevent  surv   pdf hazard se.surv se.pdf
#> 1      1    0-368    13     0  13.0      5 1.000 0.001  0.001   0.000  0.000
#> 2      1  368-476     8     2   7.0      1 0.615 0.001  0.001   0.135  0.001
#> 3      1  476-795     5     1   4.5      1 0.527 0.000  0.001   0.141  0.000
#> 4      1 795-1227     3     3   1.5      0 0.410    NA     NA   0.151     NA
#> 5      2    0-368    13     0  13.0      2 1.000 0.000  0.000   0.000  0.000
#> 6      2  368-476    11     2  10.0      2 0.846 0.002  0.002   0.100  0.001
#> 7      2  476-795     7     3   5.5      1 0.677 0.000  0.001   0.134  0.000
#> 8      2 795-1227     3     3   1.5      0 0.554    NA     NA   0.156     NA
#>   se.hazard
#> 1     0.001
#> 2     0.001
#> 3     0.001
#> 4        NA
#> 5     0.000
#> 6     0.001
#> 7     0.001
#> 8        NA
lifetable(obj, ntimes = 4, times = c(200, 500, 800, 1000))
#>   strata    times nsubs nlost nrisk nevent  surv   pdf hazard se.surv se.pdf
#> 1   rx=1    0-200    13     0  13.0      3 1.000 0.001  0.001   0.000  0.001
#> 2   rx=1  200-500    10     2   9.0      3 0.769 0.001  0.001   0.117  0.000
#> 3   rx=1  500-800     5     1   4.5      1 0.513 0.000  0.001   0.144  0.000
#> 4   rx=1 800-1000     3     3   1.5      0 0.399    NA     NA   0.150     NA
#> 5   rx=2    0-200    13     0  13.0      1 1.000 0.000  0.000   0.000  0.000
#> 6   rx=2  200-500    12     2  11.0      3 0.923 0.001  0.001   0.074  0.000
#> 7   rx=2  500-800     7     3   5.5      1 0.671 0.000  0.001   0.135  0.000
#> 8   rx=2 800-1000     3     3   1.5      0 0.549    NA     NA   0.156     NA
#>   se.hazard
#> 1     0.001
#> 2     0.001
#> 3     0.001
#> 4        NA
#> 5     0.000
#> 6     0.001
#> 7     0.001
#> 8        NA
```
