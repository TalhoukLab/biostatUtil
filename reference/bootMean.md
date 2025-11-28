# Mean and bootstrap confidence interval

Finds mean of a vector, with bootstrapped confidence bounds.

## Usage

``` r
bootMean(x, num.boot = 1000, conf.level = 0.95, seed = 12, ...)
```

## Arguments

- x:

  a vector (or matrix)

- num.boot:

  number of bootstrap samples. Defaults to 1000.

- conf.level:

  confidence level.

- seed:

  random seed for resampling

- ...:

  additional arguments to `mean`

## Value

A list with elements

- obs.mean:

  mean

- ci:

  bootstraped confidence interval

- n:

  number of bootstrap samples used

## Details

Takes a numeric vector and resamples with replacement `num.boot` times.
If the input vector has any `NA` entries, include the argument
`na.rm = TRUE` from `mean`.

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
## Vectors
set.seed(344)
bootMean(rnorm(100, 5, 3))
#> $obs.mean
#> [1] 4.906494
#> 
#> $ci
#> [1] 4.381528 5.387150
#> 
#> $n
#> [1] 100
#> 
bootMean(rnorm(100, 5, 3), num.boot = 500)
#> $obs.mean
#> [1] 4.906494
#> 
#> $ci
#> [1] 4.381528 5.393610
#> 
#> $n
#> [1] 100
#> 
bootMean(rnorm(100, 4, 3), conf.level = 0.90)
#> $obs.mean
#> [1] 3.906494
#> 
#> $ci
#> [1] 3.468191 4.309403
#> 
#> $n
#> [1] 100
#> 

## Missing Values
set.seed(344)
s <- ifelse(rexp(100, 0.5) < 1, NA, rexp(100, 0.5))
bootMean(s)  # doesn't work
#> $obs.mean
#> [1] NA
#> 
#> $ci
#> [1] 1.577629 2.419438
#> 
#> $n
#> [1] 100
#> 
bootMean(s, na.rm = TRUE)
#> $obs.mean
#> [1] 2.004357
#> 
#> $ci
#> [1] 1.577629 2.419438
#> 
#> $n
#> [1] 100
#> 
```
