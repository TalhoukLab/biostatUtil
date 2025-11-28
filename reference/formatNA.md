# Missing Value Formatting

Takes a numeric vector and replaces all missing codes with NA and
returns a factor if the variable is categorical or a numeric variable if
it's numeric.

## Usage

``` r
formatNA(y, type = c("cat", "cont"), codes = c("", "Unk", "N/A"))
```

## Arguments

- y:

  a vector.

- type:

  whether the variable is `"cat"` (categorical) or `"cont"`
  (continuous). Defaults to `"cat"`.

- codes:

  vector of missing codes to replace with `NA`

## Value

A categorical or numerical vector with all missing formatted as `NA`.

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
y <- c(1:10, "Unk", 12)
formatNA(y)
#>  [1] 1    2    3    4    5    6    7    8    9    10   <NA> 12  
#> Levels: 1 10 12 2 3 4 5 6 7 8 9
```
