# Simple capitalization

Capitalize the first letter of every word in a character string.

## Usage

``` r
simpleCap(x, first.only = FALSE)
```

## Arguments

- x:

  character string

- first.only:

  logical; if `TRUE`, only the first word will be capitalized

## Value

A character string with every word's first letter capitalized.

## Details

To capitalize only the first word, use `first.only = TRUE`.

## References

<http://stackoverflow.com/questions/6364783/capitalize-the-first-letter-of-both-words-in-a-two-word-string>

## Author

Samuel Leung

## Examples

``` r
simpleCap("clear cell")
#> [1] "Clear Cell"
simpleCap("high grade serous carcinoma")
#> [1] "High Grade Serous Carcinoma"
simpleCap("ovarian cancer", first.only = TRUE)
#> [1] "Ovarian cancer"
```
