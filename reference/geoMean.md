# Geometric mean

Calculates the geometric mean

## Usage

``` r
geoMean(x, na.rm = FALSE)
```

## Arguments

- x:

  a vector. Matrices are allowed.

- na.rm:

  logical; should NA values be removed?

## Value

If `x` has any `NA` values then `geoMean` will return `NA` just like
`mean` unless `na.rm = TRUE` is specified. Otherwise, the geometric mean
of `x` is returned. If the input only has `NA` values then `NA` is
returned as well.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
set.seed(1)
x <- rexp(100, rate = 0.5)
y <- rexp(100, rate = 0.3)
y[3] <- NA
geoMean(x)
#> [1] 1.357882
geoMean(y)
#> [1] NA
geoMean(y, na.rm = TRUE)
#> [1] 2.091611
```
