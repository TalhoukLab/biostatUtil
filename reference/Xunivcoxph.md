# Univariate cox proprtional hazards model

Concatenates hazard ratios and confidence limits for every covariate in
a Cox model.

## Usage

``` r
Xunivcoxph(mod, digits = 3)
```

## Arguments

- mod:

  model fit object, returned from either `coxph` or `coxphf`.

- digits:

  number of digits to round

## Value

Hazard ratio and 95/% confidence interval

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
library(survival)
library(coxphf)
# One predictor
test1 <- list(
  time = c(4, 3, 1, 1, 2, 2, 3),
  status = c(1, 1, 1, 0, 1, 1, 0),
  x = c(0, 2, 1, 1, 1, 0, 0),
  sex = c(0, 0, 0, 0, 1, 1, 1)
)
mod <- coxph(Surv(time, status) ~ x + strata(sex), test1)
Xunivcoxph(mod)
#> [1] "HR = 2.231 (95% CI: 0.445-11.18)"

# Multiple predictors
bladder1 <- bladder[bladder$enum < 5, ]
mod <- coxph(Surv(stop, event) ~ (rx + size + number) * strata(enum) +
cluster(id), bladder1)
Xunivcoxph(mod, digits = 2)
#>  [1] "HR = 0.59 (95% CI: 0.32-1.1)"  "HR = 1.07 (95% CI: 0.9-1.28)" 
#>  [3] "HR = 1.27 (95% CI: 1.1-1.47)"  "HR = 0.9 (95% CI: 0.47-1.73)" 
#>  [5] "HR = 0.84 (95% CI: 0.39-1.84)" "HR = 0.9 (95% CI: 0.33-2.42)" 
#>  [7] "HR = 0.86 (95% CI: 0.69-1.08)" "HR = 0.75 (95% CI: 0.56-1.01)"
#>  [9] "HR = 0.76 (95% CI: 0.52-1.1)"  "HR = 0.9 (95% CI: 0.72-1.14)" 
#> [11] "HR = 0.94 (95% CI: 0.74-1.19)" "HR = 1.1 (95% CI: 0.87-1.39)" 

# Firth's correction
test2 <- data.frame(list(
  start = c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
  stop = c(2, 3, 6, 7, 8, 9, 9, 9, 14, 17),
  event = c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
  x    = c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0)
))
mod <- coxphf(formula = Surv(start, stop, event) ~ x, pl = FALSE,
data = test2)
Xunivcoxph(mod)
#> [1] "HR^(F) = 0.886 (95% CI: 0.188-4.183)"
```
