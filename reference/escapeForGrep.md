# Escape string for regular expression

Escape `[`, `]`, `(`, and `)` for use in `grep`.

## Usage

``` r
escapeForGrep(x)
```

## Arguments

- x:

  a character vector

## Value

A character vector with opening and closing square brackets and
parentheses escaped for use in `grep`.

## Author

Samuel Leung

## Examples

``` r
escapeForGrep("[index]")
#> [1] "\\[index\\]"
escapeForGrep("(parentheses)")
#> [1] "\\(parentheses\\)"
```
