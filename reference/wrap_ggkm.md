# Wrap list of `ggkm` plots into a single patchwork

Kaplan-Meier plots generated for multiple survival outcomes can be
wrapped into a single figure.

## Usage

``` r
wrap_ggkm(x, ...)
```

## Arguments

- x:

  list of
  [`ggkm()`](https://talhouklab.github.io/biostatUtil/reference/ggkm.md)
  figures

- ...:

  additional annotation parameters passed to
  [`patchwork::plot_annotation()`](https://patchwork.data-imaginist.com/reference/plot_annotation.html)
