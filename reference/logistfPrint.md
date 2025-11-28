# Print summaries from logistf

Print nice summaries from objects returned by `logistf`.

## Usage

``` r
logistfPrint(fit, digits = 3)
```

## Arguments

- fit:

  a fit object returned by `logistf`

- digits:

  number of digits to round to

## Value

A data frame with a row for each predictor and the following columns

- coef:

  log odds ratio

- exp(coef):

  odds ratio

- lower:

  lower confidence bound of odds ratio for specified level

- upper:

  upper confidence bound of odds ratio for specified level

- p:

  p-value

## Details

There is a lot of raw output from `logistf`, and it is not easy to
extract the coefficient table. This function provides a convenient
wrapper to return summaries from `logistf`.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
library(logistf)
data(sex2)
fit <- logistf(case ~ age + oc + vic + vicl + vis + dia, data = sex2,
alpha = 0.1)
summary(fit)
#> logistf(formula = case ~ age + oc + vic + vicl + vis + dia, data = sex2, 
#>     alpha = 0.1)
#> 
#> Model fitted by Penalized ML
#> Coefficients:
#>                    coef  se(coef)  lower 0.9  upper 0.9       Chisq
#> (Intercept)  0.12025405 0.4763429 -0.6673577  0.9177303  0.06286298
#> age         -1.10598131 0.4149021 -1.8277045 -0.4328451  7.50773092
#> oc          -0.06881673 0.4344026 -0.7986318  0.6507581  0.02467044
#> vic          2.26887464 0.5384872  1.4248338  3.2325326 22.93139022
#> vicl        -2.11140817 0.5320395 -3.0613215 -1.2704704 19.10407252
#> vis         -0.78831694 0.4089620 -1.4743338 -0.1137542  3.69740975
#> dia          3.09601166 1.5052197  1.0835108  6.8752016  7.89693139
#>                        p method
#> (Intercept) 8.020268e-01      2
#> age         6.143472e-03      2
#> oc          8.751911e-01      2
#> vic         1.678877e-06      2
#> vicl        1.237805e-05      2
#> vis         5.449701e-02      2
#> dia         4.951873e-03      2
#> 
#> Method: 1-Wald, 2-Profile penalized log-likelihood, 3-None
#> 
#> Likelihood ratio test=49.09064 on 6 df, p=7.15089e-09, n=239
#> Wald test = 31.96835 on 6 df, p = 1.654713e-05

## Streamlined summary
logistfPrint(fit)
#>               coef exp(coef) lower 0.9 upper 0.9     p
#> (Intercept)  0.120     1.128     0.513     2.504 0.802
#> age         -1.106     0.331     0.161     0.649 0.006
#> oc          -0.069     0.933     0.450     1.917 0.875
#> vic          2.269     9.669     4.157    25.344 0.000
#> vicl        -2.111     0.121     0.047     0.281 0.000
#> vis         -0.788     0.455     0.229     0.892 0.054
#> dia          3.096    22.110     2.955   967.970 0.005
```
