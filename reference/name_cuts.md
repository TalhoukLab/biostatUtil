# Name cutpoint variables

Create a name for cutpoint variables based on location of cut and number
of groups.

## Usage

``` r
name_cuts(x, cuts)
```

## Arguments

- x:

  vector to split by certain cutpoints

- cuts:

  where to cut `x`

## Value

A character string representing the cutpoint variable name.

## Details

The naming system is based on how
[`Hmisc::cut2()`](https://rdrr.io/pkg/Hmisc/man/cut2.html) cuts
variables. Used in
[`build_cuts()`](https://talhouklab.github.io/biostatUtil/reference/build_cuts.md)
for naming the new cutpoint variables.

## See also

[`Hmisc::cut2()`](https://rdrr.io/pkg/Hmisc/man/cut2.html),
[`build_cuts()`](https://talhouklab.github.io/biostatUtil/reference/build_cuts.md)

## Author

Derek Chiu

## Examples

``` r
set.seed(1108)
x <- sample(0:4, size = 1000, replace = TRUE)
name_cuts(x, c(1, 4))
#> [1] "b0v1234"
name_cuts(x, c(2, 4))
#> [1] "b01v234"
```
