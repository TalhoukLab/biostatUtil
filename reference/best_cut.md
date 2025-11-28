# Find the best cutpoint for a covariate

Repeatedly finds cutpoints for an explanatory variable in a univariable
Cox PH model. Also plots survival curves for each cutpoint.

## Usage

``` r
best_cut(
  f,
  d,
  n = c("b", "t", "qd", "qn"),
  AIC.range = 3,
  nround = 3,
  plot = TRUE,
  filename = NULL,
  nrow = NULL,
  ncol = NULL,
  title = "",
  ...
)
```

## Arguments

- f:

  formula object

- d:

  data frame

- n:

  number of groups to transform variable into. Options are "b" (two),
  "t" (three), "qd" (four), and "qn" (five)

- AIC.range:

  If range of AIC is within `AIC.range` units, the likelihood is too
  flat. We choose the best cutpoint using the alternative method.

- nround:

  number of digits to round AIC and p-value on plots

- plot:

  logical; If `TRUE`, shows the survival curves for each cutpoint in a
  facetted figure

- filename:

  file name for saving a png image of figure

- nrow:

  number of rows in facetted plot

- ncol:

  number of columns in facetted plot

- title:

  title for plot

- ...:

  additional arguments for `plot`

## Value

A list with the following elements

- cuts:

  vector of cutpoints considered

- fits:

  A list of `coxph` objects run for each cutpoint

- results:

  A table showing the likelihood ratio test p-value, log likelihood, and
  AIC for each cutpoint

- opt.cut:

  optimal cutpoint value

- flat.lik:

  If `TRUE`, the likelihood was too flat and the alternative method was
  used

Additionally, if `plot = TRUE`, the function also returns KM survival
curves for each possible cutpoint.

## Details

Takes the cutpoint resulting in the lowest AIC. If the range of AIC
values is within `AIC.range` units, take the cutpoint that results in
the two groups having the most similar numbers of events and cases. The
function can cut a variable into anywhere from 2 to 5 groups.

## Author

Derek Chiu
