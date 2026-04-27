# Wrap list of `ggkm` plots into a single patchwork

Kaplan-Meier plots generated for multiple survival outcomes can be
wrapped into a single figure.

## Usage

``` r
wrap_ggkm(x, ncol = 1, nrow = NULL, ...)
```

## Arguments

- x:

  list of
  [`ggkm()`](https://talhouklab.github.io/biostatUtil/reference/ggkm.md)
  figures

- ncol:

  number of columns to wrap plots into. Default is 1.

- nrow:

  number of rows to wrap plots into.

- ...:

  additional annotation parameters passed to
  [`patchwork::plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html)
