# Is the first letter uppercase?

Returns a logical indicating whether the first letter of a character
string is uppercase.

## Usage

``` r
isFirstLetterUpperCase(x)
```

## Arguments

- x:

  character string

## Value

logical; if `TRUE`, the first letter of the input string is uppercase.

## Details

If the input is an empty string, the function returns `TRUE`.

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
isFirstLetterUpperCase("peanut butter")
#> [1] FALSE
isFirstLetterUpperCase("peanut Butter")
#> [1] FALSE
isFirstLetterUpperCase("Samuel butter")
#> [1] TRUE
isFirstLetterUpperCase("")
#> [1] TRUE
```
