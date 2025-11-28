# Nicer outputs from coxph model fits

Formats the coxph (or coxphf) model outputs into a prettier display.

## Usage

``` r
prettyCoxph(
  input.formula,
  input.d,
  ref.grp = NULL,
  use.firth = 1,
  check.ph = FALSE,
  plot.ph = TRUE,
  ph.test.plot.filename = NULL,
  ...
)
```

## Arguments

- input.formula:

  a formula object, with the response on the left of a `~` operator, and
  the terms on the right. The response must be a survival object as
  returned by the `Surv` function.

- input.d:

  data.frame containing the variables in `input.formula`

- ref.grp:

  a named array indicating reference group(s) for any variables in the
  cox model. this parameter will be ignored if not applicable, e.g. for
  continuous variable

- use.firth:

  if set to -1, Firth's correction is applied using `coxphf`. The
  default value is 1, which uses `coxph`.

- check.ph:

  if `TRUE`, checks the proportional hazard assumption.

- plot.ph:

  if `TRUE` (default), the PH residual plot is graphed.

- ph.test.plot.filename:

  character string for filename of residual plot, including file
  extension. If `NULL` (default), plots to console. Otherwise plot is
  saved to disk.

- ...:

  additional arguments to `coxph` or `coxphf`

## Value

A list with elements

- output:

  A character matrix showing the hazard ratio, confidence bounds, and
  p-value for each coefficient. If `check.ph = TRUE`, the last column
  shows the results of the PH test, otherwise it reads "NOT CALCULATED"
  for every coefficient.

- fit:

  The fit output call returned from `coxph` or `coxphf`

- n:

  The number of observations used in the fit

- nevent:

  The number of events used in the fit

- ph.test:

  The rho, Chi-squared statistic, and p-value for each coefficient. Not
  shown if `check.ph = FALSE`

- used.firth:

  logical; if `TRUE`, Firth's correction was applied using `coxphf`

## Details

The original fit call is returned, along with other useful statistics.
If Firth is called, we first determine if the percentage of censored
cases is large enough to use Firth.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
library(survival)
# Base output
test1 <- list(time = c(4, 3, 1, 1, 2, 2, 3),
status = c(1, 1, 1, 0, 1, 1, 0),
x = c(0, 2, 1, 1, 1, 0, 0),
sex = c(0, 0, 0, 0, 1, 1, 1))
coxph(Surv(time, status) ~ x + strata(sex), test1)
#> Call:
#> coxph(formula = Surv(time, status) ~ x + strata(sex), data = test1)
#> 
#>     coef exp(coef) se(coef)     z     p
#> x 0.8023    2.2307   0.8224 0.976 0.329
#> 
#> Likelihood ratio test=1.09  on 1 df, p=0.2971
#> n= 7, number of events= 5 

# Pretty output
prettyCoxph(Surv(time, status) ~ x + strata(sex), test1)
#> $output
#>   estimate  conf.low conf.high   p.value       ph.check
#> x 2.230706 0.4450758  11.18022 0.3292583 NOT CALCULATED
#> 
#> $fit
#> Call:
#> coxph(formula = .my.formula, data = .my.data)
#> 
#>     coef exp(coef) se(coef)     z     p
#> x 0.8023    2.2307   0.8224 0.976 0.329
#> 
#> Likelihood ratio test=1.09  on 1 df, p=0.2971
#> n= 7, number of events= 5 
#> 
#> $fit.firth
#> [1] NA
#> 
#> $n
#> [1] 7
#> 
#> $nevent
#> [1] 5
#> 
#> $ph.test
#> [1] NA
#> 
#> $used.firth
#> [1] FALSE
#> 

# x is now a factor
test1$x <- factor(test1$x)
prettyCoxph(Surv(time, status) ~ x + strata(sex), test1)
#> $output
#>    estimate  conf.low conf.high   p.value       ph.check
#> x1 4.685663 0.3636156  60.38089 0.2363079 NOT CALCULATED
#> x2 3.220454 0.1014294 102.25168 0.5074001 NOT CALCULATED
#> 
#> $fit
#> Call:
#> coxph(formula = .my.formula, data = .my.data)
#> 
#>     coef exp(coef) se(coef)     z     p
#> x1 1.545     4.686    1.304 1.184 0.236
#> x2 1.170     3.220    1.764 0.663 0.507
#> 
#> Likelihood ratio test=1.67  on 2 df, p=0.4342
#> n= 7, number of events= 5 
#> 
#> $fit.firth
#> [1] NA
#> 
#> $n
#> [1] 7
#> 
#> $nevent
#> [1] 5
#> 
#> $ph.test
#> [1] NA
#> 
#> $used.firth
#> [1] FALSE
#> 

# Releveled reference group
prettyCoxph(Surv(time, status) ~ x + strata(sex), test1, ref.grp =
setNames("2", "x"))
#> $output
#>     estimate   conf.low conf.high   p.value       ph.check
#> x0 0.3105152 0.00977979  9.859076 0.5074001 NOT CALCULATED
#> x1 1.4549695 0.04188378 50.543101 0.8358880 NOT CALCULATED
#> 
#> $fit
#> Call:
#> coxph(formula = .my.formula, data = .my.data)
#> 
#>       coef exp(coef) se(coef)      z     p
#> x0 -1.1695    0.3105   1.7643 -0.663 0.507
#> x1  0.3750    1.4550   1.8102  0.207 0.836
#> 
#> Likelihood ratio test=1.67  on 2 df, p=0.4342
#> n= 7, number of events= 5 
#> 
#> $fit.firth
#> [1] NA
#> 
#> $n
#> [1] 7
#> 
#> $nevent
#> [1] 5
#> 
#> $ph.test
#> [1] NA
#> 
#> $used.firth
#> [1] FALSE
#> 

# Use Firth's correction
prettyCoxph(Surv(time, status) ~ x + strata(sex), test1, use.firth = -1)
#> $output
#>                   estimate   conf.low conf.high   p.value       ph.check
#> x1               4.9610519 0.56823954  71.53693 0.1451405 NOT CALCULATED
#> x2               1.9696916 0.13404776  39.18328 0.6041722 NOT CALCULATED
#> strata(sex)sex=1 0.9098196 0.07179951  11.88728 0.9365575 NOT CALCULATED
#> 
#> $fit
#> Call:
#> coxph(formula = .my.formula, data = .my.data)
#> 
#>     coef exp(coef) se(coef)     z     p
#> x1 1.545     4.686    1.304 1.184 0.236
#> x2 1.170     3.220    1.764 0.663 0.507
#> 
#> Likelihood ratio test=1.67  on 2 df, p=0.4342
#> n= 7, number of events= 5 
#> 
#> $fit.firth
#> coxphf::coxphf(formula = .my.formula, data = .my.data, maxit = 100)
#> Model fitted by Penalized ML
#> Confidence intervals and p-values by Profile Likelihood 
#> 
#>                         coef se(coef) exp(coef) lower 0.95 upper 0.95
#> x1                1.60161779 1.354154 4.9610519 0.56823954   71.53693
#> x2                0.67787698 1.551143 1.9696916 0.13404776   39.18328
#> strata(sex)sex=1 -0.09450898 1.380503 0.9098196 0.07179951   11.88728
#>                        Chisq         p
#> x1               2.122594512 0.1451405
#> x2               0.268749031 0.6041722
#> strata(sex)sex=1 0.006335744 0.9365575
#> 
#> Likelihood ratio test=2.170314 on 3 df, p=0.5378191, n=7
#> 
#> 
#> $n
#> [1] 7
#> 
#> $nevent
#> [1] 5
#> 
#> $ph.test
#> [1] NA
#> 
#> $used.firth
#> [1] TRUE
#> 
```
