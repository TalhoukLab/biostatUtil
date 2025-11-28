# Pairwise Correlations

Computes all pairwise correlations between the columns of a data frame

## Usage

``` r
pairwiseCor(x)
```

## Arguments

- x:

  A data frame containing numeric variables of interest.

## Value

all pairwise absolute correlations, correlations, Pval, Adj P val by
decreasing order of absolute correlations.

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
set.seed(123)
x <- data.frame(matrix(rnorm(25), nrow = 5))
pairwiseCor(x)
#>    Variable1 Variable2 AbsCor     Cor   Pval   AdjP
#> 1         X1        X4 0.9392 -0.9392 0.0179 0.1785
#> 2         X2        X4 0.8429  0.8429 0.0730 0.2925
#> 3         X1        X2 0.8219 -0.8219 0.0878 0.2925
#> 4         X2        X3 0.6714  0.6714 0.2146 0.5365
#> 5         X3        X4 0.4850  0.4850 0.4077 0.7349
#> 6         X3        X5 0.4553 -0.4553 0.4410 0.7349
#> 7         X1        X5 0.3339 -0.3339 0.5829 0.8327
#> 8         X1        X3 0.2440 -0.2440 0.6924 0.8655
#> 9         X4        X5 0.0958  0.0958 0.8782 0.9758
#> 10        X2        X5 0.0107 -0.0107 0.9864 0.9864
```
