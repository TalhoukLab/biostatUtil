# Find cutpoint by Cox model statistics

Try to find a single cutpoint by maximizing the hazard ratio or
minimizing the AIC.

## Usage

``` r
findCutpointByCoxph(input.d, surv.formula)
```

## Arguments

- input.d:

  input data, typically a matrix or data frame

- surv.formula:

  a formula of type `Surv(time, status) ~ x`, where `x` is the variable
  of interest.

## Value

best cutpoint as determined by different metrics. Returns `NA` if
`surv.formula` is not univariable, the variable is not numeric, or there
is no variation in the variable of interest.

## Details

The formula must be univariable.

## Warning

`surv.formula` cannot be multivariable. For example,
`Surv(time, status) ~ x + age` won't work but `Surv(time, status) ~ x`
is fine.

## Author

Samuel Leung, Derek Chiu
