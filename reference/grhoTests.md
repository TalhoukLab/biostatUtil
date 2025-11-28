# Summary of Survival Curve differences using G-rho tests

Runs the log-rank test and Breslow test to test for difference between
two or more survival curves.

## Usage

``` r
grhoTests(formula, data, digits = 4)
```

## Arguments

- formula:

  formula expression of the form `Surv(time, status) ~ predictors`

- data:

  data frame where variables from `formula` originate

- digits:

  number of significant digits to retain

## Value

The Chi-Square statistic, degrees of freedom, and p-value are given for
both G-rho tests.

## Details

The log-rank test corresponds to `rho = 0` and the Breslow test
corresponds to `rho = 1` in `survdiff`. This function emulates the
"Overall Comparisons" table output from SPSS.

## Author

Derek Chiu

## Examples

``` r
library(survival)
grhoTests(Surv(futime, fustat) ~ rx, data = ovarian)
#>                                Chi-Square df   Sig.
#> Log Rank (Mantel-Cox)               1.063  1 0.3026
#> Breslow (Generalized Wilcoxon)      1.685  1 0.1943
```
