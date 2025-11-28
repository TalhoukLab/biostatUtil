# Confusion matrix results in HTML

Prints results from
[`binaryCM()`](https://talhouklab.github.io/biostatUtil/reference/binaryCM.md)
into a nice HTML table format.

## Usage

``` r
binaryCMAsHTML(
  x,
  y,
  ref.description = NULL,
  digits = 4,
  seed = 20,
  num.boot = 1000,
  conf.level = 0.95,
  show.ci = TRUE
)
```

## Arguments

- x:

  vector of reference classes

- y:

  vector of predicted classes

- ref.description:

  description of classes

- digits:

  number of digits to round p-values to

- seed:

  random seed for bootstrap resampling

- num.boot:

  number of bootstrap confidence intervals

- conf.level:

  confidence level. Defaults to 95%.

- show.ci:

  if `TRUE` (default), the confidence intervals are shown.

## Value

A character string that can be parsed as HTML code to display a nice
confusion matrix summary.

## See also

Other confusion matrix functions:
[`binaryCM()`](https://talhouklab.github.io/biostatUtil/reference/binaryCM.md),
[`multiClassCM()`](https://talhouklab.github.io/biostatUtil/reference/multiClassCM.md)

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
# 95% CI from 5 bootstraped samples
library(htmlTable)
set.seed(547)
n <- 20
x <- rbinom(n, size = 1, prob = 0.6)
y <- rbinom(n, size = 1, prob = 0.4)
results <- binaryCMAsHTML(x, y, "Test", num.boot = 1000)
htmlTable(results)
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;'  id='table_3'>
#> <tbody>
#> <tr style='border-top: 2px solid grey;'>
#> <td style='border-top: 2px solid grey; border-bottom: 2px solid grey; text-align: center;'><table><tr><td style="text-align: right; white-space: nowrap;">Reference:</td><td style="text-align: left; white-space: nowrap;">Test</td></tr><tr><td style="text-align: right; white-space: nowrap;">Accuracy (95% CI):</td><td style="text-align: left; white-space: nowrap;">0.55 (0.3421 - 0.7418)</td></tr><tr><td style="text-align: right; white-space: nowrap;">Sensitivity (95% CI):</td><td style="text-align: left; white-space: nowrap;">0.4444 (0.1888 - 0.7333)</td></tr><tr><td style="text-align: right; white-space: nowrap;">Specificity (95% CI):</td><td style="text-align: left; white-space: nowrap;">0.6364 (0.3538 - 0.8483)</td></tr><tr><td style="text-align: right; white-space: nowrap;">PPV (95% CI):</td><td style="text-align: left; white-space: nowrap;">0.5 (0.2152 - 0.7848)</td></tr><tr><td style="text-align: right; white-space: nowrap;">NPV (95% CI):</td><td style="text-align: left; white-space: nowrap;">0.5833 (0.3195 - 0.8067)</td></tr><tr><td style="text-align: right; white-space: nowrap;">kappa (95% CI):</td><td style="text-align: left; white-space: nowrap;">0.0816 (-0.4 - 0.4898)</td></tr></table></td>
#> </tr>
#> </tbody>
#> </table>

results.no.ci <- binaryCMAsHTML(x, y, "Test", num.boot = 1000, show.ci =
FALSE)
htmlTable(results.no.ci)
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;'  id='table_4'>
#> <tbody>
#> <tr style='border-top: 2px solid grey;'>
#> <td style='border-top: 2px solid grey; border-bottom: 2px solid grey; text-align: center;'><table><tr><td style="text-align: right; white-space: nowrap;">Reference:</td><td style="text-align: left; white-space: nowrap;">Test</td></tr><tr><td style="text-align: right; white-space: nowrap;">Accuracy:</td><td style="text-align: left; white-space: nowrap;">0.55</td></tr><tr><td style="text-align: right; white-space: nowrap;">Sensitivity:</td><td style="text-align: left; white-space: nowrap;">0.4444</td></tr><tr><td style="text-align: right; white-space: nowrap;">Specificity:</td><td style="text-align: left; white-space: nowrap;">0.6364</td></tr><tr><td style="text-align: right; white-space: nowrap;">PPV:</td><td style="text-align: left; white-space: nowrap;">0.5</td></tr><tr><td style="text-align: right; white-space: nowrap;">NPV:</td><td style="text-align: left; white-space: nowrap;">0.5833</td></tr><tr><td style="text-align: right; white-space: nowrap;">kappa:</td><td style="text-align: left; white-space: nowrap;">0.0816</td></tr></table></td>
#> </tr>
#> </tbody>
#> </table>
```
