# Nice output from Cox regression object

Nicely formatted output from a Cox regression object, either `coxph` or
`coxphf`.

## Usage

``` r
coxphOut(object, coefnames = NULL, conf.level = 0.95, digits = 2)
```

## Arguments

- object:

  a model fit object returned from `coxph` or `coxphf`

- coefnames:

  a vector of labels for the coefficient names returned by the fit.
  `NULL` by default, uses original coefficient names.

- conf.level:

  confidence level for hazard ratio. Defaults to 95%.

- digits:

  number of digits to round to

## Value

A matrix with a row for each coefficient, and a column for each of the
following elements

- n:

  the number of observations used in the fit.

- events:

  the number of events used in the fit.

- coef:

  log hazard ratio

- se:

  standard error of `coef`

- Z-Score:

  Wald test statistic

- P-value:

  p-value for `Z-Score`

- HR:

  hazard ratio

- Lower CI Limit:

  Defaults to `2.5 \%`

- Upper CI Limit:

  Defaults to `97.5 \%`

## Details

For objects of class `coxphf`, the calculation of the number of events
used in the fit is slightly different.

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
library(survival)
test1 <- list(time = c(4, 3, 1, 1, 2, 2, 3),
status = c(1, 1, 1, 0, 1, 1, 0),
x = c(0, 2, 1, 1, 1, 0, 0),
sex = c(0, 0, 0, 0, 1, 1, 1))

# Stratified
mod1 <- coxph(Surv(time, status) ~ x + strata(sex), test1)
coxphOut(mod1)
#>   n events coef   se Z-Score P-value   HR 2.5 % 97.5 %
#> x 7      5  0.8 0.82    0.98    0.33 2.23  0.45  11.18

# Not stratified
mod2 <- coxph(Surv(time, status) ~ x + sex, test1)
coxphOut(mod2, coefnames = c("x", "gender"))
#>        n events coef   se Z-Score P-value   HR 2.5 % 97.5 %
#> x      7      5 0.78 0.80    0.98    0.33 2.18  0.46  10.43
#> gender 7      5 0.93 1.41    0.66    0.51 2.54  0.16  40.19
```
