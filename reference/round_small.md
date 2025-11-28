# Rounding of Small Numbers

Rounding of a number smaller than specified precision doesn't coerce to
0.

## Usage

``` r
round_small(x, method = c("round", "signif"), digits = 3, sci = FALSE)
```

## Arguments

- x:

  a numeric vector or matrix

- method:

  use either [`round()`](https://rdrr.io/r/base/Round.html) or
  [`signif()`](https://rdrr.io/r/base/Round.html) as rounding method

- digits:

  integer indicating number of decimal places to round to

- sci:

  if `TRUE`, scientific notation is used

## Value

If precision of number is larger than desired rounding, the default
`round` is used. Otherwise, we provide an upper bound instead of
coercion to 0.

## Details

This function is useful when we have small p-values and don't want to
show the scientific notation, or coercion to 0. Instead we show an upper
bound. For example, if a p-value is 2e-05 and we want to round to 3
digits, the function will return "\< 0.001".

## Author

Derek Chiu

## Examples

``` r
# Vector inputs
round_small(2e-04)
#> [1] "< 0.001"
round_small(5e-04)
#> [1] "< 0.001"
round_small(6e-04)
#> [1] 0.001

# Matrix input
set.seed(12)
x <- matrix(rexp(25, 3), nrow = 5)
round_small(x, digits = 1)
#>      [,1]    [,2]  [,3]  [,4]  [,5] 
#> [1,] "0.7"   "0.1" "0.4" "0.7" "0.3"
#> [2,] "0.2"   "1.3" "0.4" "0.9" "0.2"
#> [3,] "< 0.1" "1.4" "0.3" "0.1" "0.1"
#> [4,] "1"     "0.4" "0.3" "0.5" "0.2"
#> [5,] "0.6"   "0.2" "0.1" "0.6" "0.4"
```
