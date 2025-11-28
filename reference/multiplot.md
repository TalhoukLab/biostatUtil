# Multiple plots

Place multiple ggplot objects on the same figure space

## Usage

``` r
multiplot(..., plotlist = NULL, cols = 1, layout = NULL)
```

## Arguments

- ...:

  pass ggplot objects

- plotlist:

  pass a list of ggplot objects

- cols:

  number of columns in layout

- layout:

  a matrix specifying the layout. If present, `cols` is ignored.

## Value

A grid object made up of multiple ggplots

## Details

If the `layout` is something like
`matrix(c(1, 2, 3, 3), nrow = 2, byrow = TRUE)`, then plot 1 will go in
the upper left, 2 will go in the upper right, and 3 will go all the way
across the bottom.

## Author

Aline Talhouk
