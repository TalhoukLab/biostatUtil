# Bootstrapped confidence interval for kappa statistic.

Function to compute cohen's kappa with binary data with a bootstrap
confidence interval

## Usage

``` r
kappaBootCI(
  x,
  y,
  seed = 20,
  num.boot = 1000,
  conf.level = 0.95,
  method = c("cohen", "weighted", "fleiss", "krippendorff"),
  type = "nominal"
)
```

## Arguments

- x:

  vector of binary scores from first rater

- y:

  vector of binary scores from second rater

- seed:

  random seed for bootstrapping

- num.boot:

  number of times to bootstrap. Defaults to 1000.

- conf.level:

  confidence level. Defaults to 95%.

- method:

  statistic to calculate bootstrap estimate

- type:

  method setting for `method = "krippendorff"`

## Value

bootstraped confidence interval for Cohen's kappa.

## Details

Cohen's kappa measures the amount of agreement between 2 raters of a
binary variable. The bootstrap confidence interval is adjusted (BCa).

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
a <- rbinom(n = 100, size = 1, prob = 0.3)
b <- rbinom(n = 100, size = 1, prob = 0.7)
kappaBootCI(a, b)
#>    PointEst        2.5%       97.5% 
#> -0.11313869 -0.30616992  0.05064322 

## Use a different seed
kappaBootCI(a, b, 5)
#>    PointEst        2.5%       97.5% 
#> -0.11313869 -0.30014328  0.03985507 
```
