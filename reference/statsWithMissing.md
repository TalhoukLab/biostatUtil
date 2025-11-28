# Calculates statistics with specifying missing values

Returns various statistics, with the ability to specify which values are
missing.

## Usage

``` r
minWithMissing(x, missing.value = -1, return.missing.value = -1)

maxWithMissing(x, missing.value = -1, return.missing.value = -1)

meanWithMissing(x, missing.value = -1, return.missing.value = -1)

sumWithMissing(x, missing.value = -1, return.missing.value = -1)

prodWithMissing(x, missing.value = -1, return.missing.value = -1)

ratioWithMissing(x, y, missing.value = -1, return.missing.value = -1)
```

## Arguments

- x, y:

  input vector

- missing.value:

  missing values in `x`

- return.missing.value:

  character to return for missing values

## Details

The statistics supported include `min, max, mean, sum, prod`, as
indicated by each function's prefix. In addition, `ratioWithMissing`
calculates the ratio `x / y`.

## Note

`NAs` are ignored.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
x <- c(10:1)
y <- c(1:10)
z <- c(10:1)
minWithMissing(z, c(1, 3))
#> [1] 2
maxWithMissing(z, c(9, 10))
#> [1] 8
meanWithMissing(z, c(3:7))
#> [1] 6
sumWithMissing(z, c(9, 10))
#> [1] 36
prodWithMissing(z, c(9, 10))
#> [1] 40320
ratioWithMissing(x, y, c(1:2))
#>  [1] -1.0000000 -1.0000000  2.6666667  1.7500000  1.2000000  0.8333333
#>  [7]  0.5714286  0.3750000 -1.0000000 -1.0000000

## All missing
minWithMissing(z, c(1:10))
#> [1] -1
minWithMissing(z, c(1:10), return.missing.value = "all missing")
#> [1] "all missing"
maxWithMissing(z, c(1:10))
#> [1] -1
maxWithMissing(z, c(1:10), return.missing.value = "all missing")
#> [1] "all missing"
meanWithMissing(z, c(1:10))
#> [1] -1
meanWithMissing(z, c(1:10), return.missing.value = "all missing")
#> [1] "all missing"
prodWithMissing(z, c(1:10))
#> [1] -1
prodWithMissing(z, c(1:10), return.missing.value = "all missing")
#> [1] "all missing"
```
