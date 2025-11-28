# Process mass spectrometry data

Process mass spectrometry data for filtering out contaminant samples,
manipulating variables, removing duplicates, and more.

## Usage

``` r
ms_process(
  psm,
  protein,
  treatment,
  samples = NULL,
  sample.id = NULL,
  path = NULL,
  ...
)
```

## Arguments

- psm:

  PSM file

- protein:

  Protein file

- treatment:

  character vector of treatment groups

- samples:

  character vector of sample names to keep

- sample.id:

  character vector for sample IDs. Order of samples must match that in
  the psm raw data.

- path:

  file path to save return element `pep`

- ...:

  additional arguments to `ms_condition`

## Value

A list with the following elements

- pep:

  processed data frame to be used by `ms_summarize`

- raw:

  raw data values

- l2:

  log2 raw data values

- vsn:

  vsn raw data values

## See also

Other Mass Spectrometry functions:
[`ms_condition()`](https://talhouklab.github.io/biostatUtil/reference/ms_condition.md),
[`ms_plot`](https://talhouklab.github.io/biostatUtil/reference/ms_plot.md),
[`ms_summarize()`](https://talhouklab.github.io/biostatUtil/reference/ms_summarize.md),
[`ms_top()`](https://talhouklab.github.io/biostatUtil/reference/ms_top.md)

## Author

Derek Chiu
