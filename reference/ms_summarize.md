# Summarize mass spectrometry data

Summarize mass spectrometry data for expression differences between
sample groups. Apply Benjamini-Hochberg p-value adjustment.

## Usage

``` r
ms_summarize(
  x,
  g,
  level = c("Gene", "Peptide"),
  col.names = NULL,
  info.vars = NULL,
  path = NULL
)
```

## Arguments

- x:

  data frame with expression values

- g:

  vector of factor levels for samples

- level:

  analysis is on the "Gene" level or "Peptide" level

- col.names:

  vector of column names for output data frame

- info.vars:

  vector of column names containing metadata information. These
  variables are collapsed if not unique.

- path:

  file path to save result object

## Value

A data frame of statistics from analyzing mass spec data. Includes
t-values, Wald p-values, effect sizes, fold change, absolute fold
change.

## Details

Gene-level analysis is performed on the "Gene" variable. Peptide-level
analysis is performed on distinct combinations of the Accession, Gene,
Descriptions, Sequence, and Modifications variables. This combined
variable is renamed to "AGDSM".

## See also

Other Mass Spectrometry functions:
[`ms_condition()`](https://talhouklab.github.io/biostatUtil/reference/ms_condition.md),
[`ms_plot`](https://talhouklab.github.io/biostatUtil/reference/ms_plot.md),
[`ms_process()`](https://talhouklab.github.io/biostatUtil/reference/ms_process.md),
[`ms_top()`](https://talhouklab.github.io/biostatUtil/reference/ms_top.md)

## Author

Derek Chiu
