# Confusion matrix summaries

Calculates summaries from cross-tabulated reference and prediction
labels for a two-class variable.

## Usage

``` r
binaryCM(
  x,
  y,
  seed = 20,
  num.boot = 1000,
  pcond = 1,
  conf.level = 0.95,
  digits = 4,
  method = "wilson",
  verbose = FALSE
)
```

## Arguments

- x:

  a vector of reference classes

- y:

  a vector of predicted classes

- seed:

  random seed for bootstrapping

- num.boot:

  number of times to bootstrap. Defaults to 1000.

- pcond:

  a string or value to be considered positive condition; defaults to 1.

- conf.level:

  confidence level. Defaults to 95%.

- digits:

  number of digits to round summaries to

- method:

  method for obtaining confidence intervals for binomial probabilities.
  See [`Hmisc::binconf`](https://rdrr.io/pkg/Hmisc/man/binconf.html) for
  details.

- verbose:

  logical; if `TRUE`, outputs are printed to the screen

## Value

A confusion matrix for the predicted and reference classes. Then the
estimated statistics along with bootstrapped confidence intervals. A
list with the following elements

- CM:

  The confusion matrix, whose columns are the predicted conditions and
  its rows are the true conditions

- Accuracy:

  Accuracy point estimate, lower bound and upper bound for bootstrapped
  CI

- Sensitivity:

  Sensitivity point estimate, lower bound and upper bound for
  bootstrapped CI

- Specificity:

  Specificity point estimate, lower bound and upper bound for
  bootstrapped CI

- PPV:

  PPV point estimate, lower bound and upper bound for bootstrapped CI

- NPV:

  NPV point estimate, lower bound and upper bound for bootstrapped CI

- kappa:

  kappa point estimate, lower bound and upper bound for bootstrapped CI

- table:

  a data frame that contains all 6 of the estimated statistics along
  with confidence intervals

## Details

Given two dichotomous variables summarized in a confusion matrix, this
function provides performance summaries. The accuracy, sensitivity,
specificity, positive predictive value (PPV), negative predictive value
(NPV), and the kappa statistic, along with their bootstrapped confidence
intervals are returned.

Note that the classes given in `x` and `y` must be binary.

## See also

Other confusion matrix functions:
[`binaryCMAsHTML()`](https://talhouklab.github.io/biostatUtil/reference/binaryCMAsHTML.md),
[`multiClassCM()`](https://talhouklab.github.io/biostatUtil/reference/multiClassCM.md)

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
### 95% CI from 1000 bootstraped samples
set.seed(547)
n <- 80
x <- rbinom(n, size = 1, prob = 0.6)
y <- rbinom(n, size = 1, prob = 0.4)
binaryCM(x, y)
#> $CM
#>          Prediction
#> Reference  1  0
#>         1 14 31
#>         0 13 22
#> 
#> $Accuracy
#>  PointEst   2.5%  97.5%
#>      0.45 0.3458 0.5588
#> 
#> $Sensitivity
#>  PointEst   2.5%  97.5%
#>    0.3111 0.1953 0.4566
#> 
#> $Specificity
#>  PointEst   2.5%  97.5%
#>    0.6286 0.4634 0.7683
#> 
#> $PPV
#>  PointEst   2.5%  97.5%
#>    0.5185 0.3399 0.6926
#> 
#> $NPV
#>  PointEst   2.5%  97.5%
#>    0.4151 0.2926 0.5491
#> 
#> $kappa
#>  PointEst    2.5%  97.5%
#>   -0.0571 -0.2437 0.1526
#> 
#> $table
#>             Point Estimate Lower CI Upper CI
#> Accuracy            0.4500   0.3458   0.5588
#> Sensitivity         0.3111   0.1953   0.4566
#> Specificity         0.6286   0.4634   0.7683
#> PPV                 0.5185   0.3399   0.6926
#> NPV                 0.4151   0.2926   0.5491
#> kappa              -0.0571  -0.2437   0.1526
#> 

### 90% CI from 500 bootstrapped samples
binaryCM(x, y, num.boot = 500, conf.level = 0.90)
#> $CM
#>          Prediction
#> Reference  1  0
#>         1 14 31
#>         0 13 22
#> 
#> $Accuracy
#>  PointEst     5%    95%
#>      0.45 0.3616 0.5416
#> 
#> $Sensitivity
#>  PointEst     5%    95%
#>    0.3111 0.2111 0.4326
#> 
#> $Specificity
#>  PointEst     5%    95%
#>    0.6286 0.4896 0.7491
#> 
#> $PPV
#>  PointEst    5%    95%
#>    0.5185 0.366 0.6676
#> 
#> $NPV
#>  PointEst     5%    95%
#>    0.4151 0.3105 0.5279
#> 
#> $kappa
#>  PointEst      5%    95%
#>   -0.0571 -0.2237 0.1247
#> 
#> $table
#>             Point Estimate Lower CI Upper CI
#> Accuracy            0.4500   0.3616   0.5416
#> Sensitivity         0.3111   0.2111   0.4326
#> Specificity         0.6286   0.4896   0.7491
#> PPV                 0.5185   0.3660   0.6676
#> NPV                 0.4151   0.3105   0.5279
#> kappa              -0.0571  -0.2237   0.1247
#> 

### Round to 2 digits
binaryCM(x, y, digits = 2)
#> $CM
#>          Prediction
#> Reference  1  0
#>         1 14 31
#>         0 13 22
#> 
#> $Accuracy
#>  PointEst 2.5% 97.5%
#>      0.45 0.35  0.56
#> 
#> $Sensitivity
#>  PointEst 2.5% 97.5%
#>      0.31  0.2  0.46
#> 
#> $Specificity
#>  PointEst 2.5% 97.5%
#>      0.63 0.46  0.77
#> 
#> $PPV
#>  PointEst 2.5% 97.5%
#>      0.52 0.34  0.69
#> 
#> $NPV
#>  PointEst 2.5% 97.5%
#>      0.42 0.29  0.55
#> 
#> $kappa
#>  PointEst  2.5% 97.5%
#>     -0.06 -0.24  0.15
#> 
#> $table
#>             Point Estimate Lower CI Upper CI
#> Accuracy              0.45     0.35     0.56
#> Sensitivity           0.31     0.20     0.46
#> Specificity           0.63     0.46     0.77
#> PPV                   0.52     0.34     0.69
#> NPV                   0.42     0.29     0.55
#> kappa                -0.06    -0.24     0.15
#> 
```
