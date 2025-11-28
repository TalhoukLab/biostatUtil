# Find occurences of string within another string

Returns a vector of the indices where a string occurs in another string

## Usage

``` r
indexOf(a, b, ignore.case = FALSE)
```

## Arguments

- a:

  string to be checked against

- b:

  string to check

- ignore.case:

  logical; if `TRUE`, case is ignored when performing the check

## Value

Indices where `b` occurs in `a`. Returns `NA` if there are no
occurences.

## Details

If `b` is longer than `a`, `indexOf` returns `NA`, since it is not
possible for a longer string to occur in a shorter string.

## See also

[`stringr::str_locate_all()`](https://stringr.tidyverse.org/reference/str_locate.html)

## Author

Samuel Leung

## Examples

``` r
indexOf("derek", "e")
#> [1] 2 4
indexOf("Animals", "a")
#> [1] 5
indexOf("Animals", "A")
#> [1] 1
indexOf("Animals", "a", ignore.case = TRUE)
#> [1] 1 5
```
