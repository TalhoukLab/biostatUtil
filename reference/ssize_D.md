# Sample Size Calculation

Sample size calculation using model-based discrimination measure.

## Usage

``` r
ssize_D(
  fit,
  D = NULL,
  cens = NULL,
  p = 0.1,
  alpha = 0.05,
  power = 0.8,
  delta = 0.25,
  trial = c("superiority", "non-inferiority")
)
```

## Arguments

- fit:

  an object fit of class `coxph`

- D:

  discrimination measure D, defaults to output of
  [`survival::royston()`](https://rdrr.io/pkg/survival/man/royston.html)
  for `fit`

- cens:

  censoring proportion, defaults to proportion calculated from `fit`

- p:

  proportion of `D` accepted as `delta`, defaults to 10%.

- alpha:

  Type I error

- power:

  statistical power (1 - Type II error)

- delta:

  superiority/non-inferiority margin. The difference we wish to detect
  between a prior model and a new model.

- trial:

  either "superiority" or "non-inferiority"

## Value

The final sample size needed in the new model using the supplied input
parameters.

## Details

The sample size calculation uses a discrimination measured based on
multivariable prognostic time-to-event models.

## References

<https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4603804/>

## Author

Derek Chiu
