# Confusion matrix summaries

Calculates summaries from cross-tabulated reference and prediction
labels for a multi-class variable.

## Usage

``` r
multiClassCM(
  x,
  y,
  seed = 20,
  num.boot = 1000,
  conf.level = 0.95,
  digits = 2,
  method = "wilson"
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

- conf.level:

  confidence level. Defaults to 95%.

- digits:

  number of digits to round summaries to

- method:

  method for obtaining confidence intervals for binomial probabilities.
  See [`Hmisc::binconf()`](https://rdrr.io/pkg/Hmisc/man/binconf.html)
  for details.

## Value

A confusion matrix for the predicted and reference classes. Then the
estimated statistics along with bootstrapped confidence intervals. A
list with the following elements

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

## Details

Given two multi-class variables summarized in a confusion matrix, this
function provides performance summaries. It provides overall accuracy
with confidence intervals, as well as per class accuracy, sensitivity,
specificity, positive predictive value (PPV), negative predictive value
(NPV). if variable entered is binary, it will automatically call
binaryCM

## See also

Other confusion matrix functions:
[`binaryCM()`](https://talhouklab.github.io/biostatUtil/reference/binaryCM.md),
[`binaryCMAsHTML()`](https://talhouklab.github.io/biostatUtil/reference/binaryCMAsHTML.md)

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
### 95% CI from 1000 bootstraped samples
set.seed(23)
k <- 3
(x <- factor(sample(1:k, 100, replace = TRUE, prob = c(0.15, 0.25, 0.6))))
#>   [1] 3 3 3 2 2 3 1 1 2 1 1 2 3 3 2 3 3 3 1 2 3 3 3 2 3 2 2 1 3 2 2 3 3 3 3 2 3
#>  [38] 1 1 3 2 3 3 2 2 2 3 3 3 2 3 2 3 2 1 3 3 1 3 1 3 3 3 3 3 3 3 1 3 1 3 2 1 3
#>  [75] 3 3 2 3 3 1 1 3 1 2 2 3 3 1 3 3 3 3 3 3 3 3 2 3 3 3
#> Levels: 1 2 3
(y <- factor(sample(1:k, 100, replace = TRUE, prob = c(0.05, 0.4, 0.65))))
#>   [1] 2 3 3 3 3 2 2 2 2 3 3 3 3 3 2 2 2 3 2 2 3 2 3 3 3 2 3 3 1 3 3 3 3 2 3 3 2
#>  [38] 2 3 3 2 3 3 3 3 3 2 3 3 3 2 2 2 2 3 2 3 2 3 2 3 3 2 3 1 2 2 2 2 3 3 2 3 2
#>  [75] 3 3 2 3 3 2 2 2 2 2 2 2 3 3 3 3 3 3 2 3 2 1 2 1 2 2
#> Levels: 1 2 3
prop.table(table(y))
#> y
#>    1    2    3 
#> 0.04 0.44 0.52 
multiClassCM(x, y)
#> $CM
#>          Prediction
#> Reference   1   2   3 Sum
#>       1     0  10   8  18
#>       2     0  12  12  24
#>       3     4  22  32  58
#>       Sum   4  44  52 100
#> 
#> $overall
#>                     Overall Concordance Statistics
#> Overall Accuracy    "0.44 (0.34 - 0.54)"          
#> Cohen's kappa       "0.04 (-0.08 - 0.19)"         
#> No Information Rate "0.58"                        
#> P-Value [Acc > NIR] "1"                           
#> 
#> $table
#>                      Average 1                    2                   
#> Sensitivity          "0.3"   "0 (0 - 0.49)"       "0.27 (0.16 - 0.42)"
#> Specificity          "0.69"  "0.81 (0.72 - 0.88)" "0.79 (0.66 - 0.87)"
#> Pos Pred Value       "0.35"  "0 (0 - 0.18)"       "0.5 (0.31 - 0.69)" 
#> Neg Pred Value       "0.68"  "0.95 (0.88 - 0.98)" "0.58 (0.47 - 0.68)"
#> Prevalence           "0.33"  "0.04 (0.02 - 0.1)"  "0.44 (0.35 - 0.54)"
#> Detection Rate       "0.15"  "0 (0 - 0.04)"       "0.12 (0.07 - 0.2)" 
#> Detection Prevalence "0.33"  "0.18 (0.12 - 0.27)" "0.24 (0.17 - 0.33)"
#> Accuracy             "0.63"  "0.78 (0.69 - 0.85)" "0.56 (0.46 - 0.65)"
#> Balanced Accuracy    "0.49"  "0.41"               "0.53"              
#>                      3                   
#> Sensitivity          "0.62 (0.48 - 0.74)"
#> Specificity          "0.46 (0.33 - 0.6)" 
#> Pos Pred Value       "0.55 (0.42 - 0.67)"
#> Neg Pred Value       "0.52 (0.38 - 0.67)"
#> Prevalence           "0.52 (0.42 - 0.62)"
#> Detection Rate       "0.32 (0.24 - 0.42)"
#> Detection Prevalence "0.58 (0.48 - 0.67)"
#> Accuracy             "0.54 (0.44 - 0.63)"
#> Balanced Accuracy    "0.54"              
#> 

### 90% CI from 500 bootstrapped samples
multiClassCM(x, y, num.boot = 500, conf.level = 0.90)
#> $CM
#>          Prediction
#> Reference   1   2   3 Sum
#>       1     0  10   8  18
#>       2     0  12  12  24
#>       3     4  22  32  58
#>       Sum   4  44  52 100
#> 
#> $overall
#>                     Overall Concordance Statistics
#> Overall Accuracy    "0.44 (0.34 - 0.54)"          
#> Cohen's kappa       "0.04 (-0.07 - 0.17)"         
#> No Information Rate "0.58"                        
#> P-Value [Acc > NIR] "1"                           
#> 
#> $table
#>                      Average 1                    2                   
#> Sensitivity          "0.3"   "0 (0 - 0.4)"        "0.27 (0.18 - 0.39)"
#> Specificity          "0.69"  "0.81 (0.74 - 0.87)" "0.79 (0.68 - 0.86)"
#> Pos Pred Value       "0.35"  "0 (0 - 0.13)"       "0.5 (0.34 - 0.66)" 
#> Neg Pred Value       "0.68"  "0.95 (0.9 - 0.98)"  "0.58 (0.48 - 0.67)"
#> Prevalence           "0.33"  "0.04 (0.02 - 0.09)" "0.44 (0.36 - 0.52)"
#> Detection Rate       "0.15"  "0 (0 - 0.03)"       "0.12 (0.08 - 0.18)"
#> Detection Prevalence "0.33"  "0.18 (0.13 - 0.25)" "0.24 (0.18 - 0.32)"
#> Accuracy             "0.63"  "0.78 (0.7 - 0.84)"  "0.56 (0.48 - 0.64)"
#> Balanced Accuracy    "0.49"  "0.41"               "0.53"              
#>                      3                   
#> Sensitivity          "0.62 (0.5 - 0.72)" 
#> Specificity          "0.46 (0.35 - 0.58)"
#> Pos Pred Value       "0.55 (0.44 - 0.65)"
#> Neg Pred Value       "0.52 (0.4 - 0.65)" 
#> Prevalence           "0.52 (0.44 - 0.6)" 
#> Detection Rate       "0.32 (0.25 - 0.4)" 
#> Detection Prevalence "0.58 (0.5 - 0.66)" 
#> Accuracy             "0.54 (0.46 - 0.62)"
#> Balanced Accuracy    "0.54"              
#> 

### Round to 2 digits
multiClassCM(x, y, digits = 2)
#> $CM
#>          Prediction
#> Reference   1   2   3 Sum
#>       1     0  10   8  18
#>       2     0  12  12  24
#>       3     4  22  32  58
#>       Sum   4  44  52 100
#> 
#> $overall
#>                     Overall Concordance Statistics
#> Overall Accuracy    "0.44 (0.34 - 0.54)"          
#> Cohen's kappa       "0.04 (-0.08 - 0.19)"         
#> No Information Rate "0.58"                        
#> P-Value [Acc > NIR] "1"                           
#> 
#> $table
#>                      Average 1                    2                   
#> Sensitivity          "0.3"   "0 (0 - 0.49)"       "0.27 (0.16 - 0.42)"
#> Specificity          "0.69"  "0.81 (0.72 - 0.88)" "0.79 (0.66 - 0.87)"
#> Pos Pred Value       "0.35"  "0 (0 - 0.18)"       "0.5 (0.31 - 0.69)" 
#> Neg Pred Value       "0.68"  "0.95 (0.88 - 0.98)" "0.58 (0.47 - 0.68)"
#> Prevalence           "0.33"  "0.04 (0.02 - 0.1)"  "0.44 (0.35 - 0.54)"
#> Detection Rate       "0.15"  "0 (0 - 0.04)"       "0.12 (0.07 - 0.2)" 
#> Detection Prevalence "0.33"  "0.18 (0.12 - 0.27)" "0.24 (0.17 - 0.33)"
#> Accuracy             "0.63"  "0.78 (0.69 - 0.85)" "0.56 (0.46 - 0.65)"
#> Balanced Accuracy    "0.49"  "0.41"               "0.53"              
#>                      3                   
#> Sensitivity          "0.62 (0.48 - 0.74)"
#> Specificity          "0.46 (0.33 - 0.6)" 
#> Pos Pred Value       "0.55 (0.42 - 0.67)"
#> Neg Pred Value       "0.52 (0.38 - 0.67)"
#> Prevalence           "0.52 (0.42 - 0.62)"
#> Detection Rate       "0.32 (0.24 - 0.42)"
#> Detection Prevalence "0.58 (0.48 - 0.67)"
#> Accuracy             "0.54 (0.44 - 0.63)"
#> Balanced Accuracy    "0.54"              
#> 
```
