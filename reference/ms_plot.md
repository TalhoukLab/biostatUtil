# Plots for MS analyses

`ms_boxplot` shows boxplots of different MS expression data values.
`ms_mean_var` shows mean-sd plots for the VSN data values to analyze the
mean-variance relationship.

## Usage

``` r
ms_boxplot(x, width = 8, height = 10, path = NULL)

ms_mean_var(x, g, title = NULL, width = 8, height = 10, path = NULL)
```

## Arguments

- x:

  data object returned by `ms_process`

- width:

  width of plot

- height:

  height of plot

- path:

  file path to save figure. Device is pdf.

- g:

  vector of treatment groups

- title:

  vector of titles for each `g`

## Value

Both functions return a pdf saved to the file location specified by
`path`. `ms_boxplot` shows three boxplots of expression values: raw data
values, log2 and vsn transformed values. `ms_mean_var` shows the vsn
transformed values and mean-sd plots for each treatment group.

## See also

Other Mass Spectrometry functions:
[`ms_condition()`](https://talhouklab.github.io/biostatUtil/reference/ms_condition.md),
[`ms_process()`](https://talhouklab.github.io/biostatUtil/reference/ms_process.md),
[`ms_summarize()`](https://talhouklab.github.io/biostatUtil/reference/ms_summarize.md),
[`ms_top()`](https://talhouklab.github.io/biostatUtil/reference/ms_top.md)

## Author

Derek Chiu
