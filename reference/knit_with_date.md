# Knit R markdown report with today's date

Append today's date to R markdown output file name. This function
modifies the default behaviour of the Knit button in R Studio. It is
intended to be passed in the YAML `knit:` field.

## Usage

``` r
knit_with_date(input, sep = "_", ...)
```

## Arguments

- input:

  input file

- sep:

  separator between input file name and date

- ...:

  additional arguments to
  [`rmarkdown::render()`](https://pkgs.rstudio.com/rmarkdown/reference/render.html)

## Value

The output file name will have today's date appended at the end.

## References

<https://bookdown.org/yihui/rmarkdown-cookbook/custom-knit.html>

## Author

Derek Chiu
