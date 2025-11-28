# Top variables in mass spectrometry analysis

Show top genes/peptides defined by significance level and absolute fold
change.

## Usage

``` r
ms_top(x, level = c("Gene", "Peptide"), alpha = 0.05, fc = 1.25, path = NULL)
```

## Arguments

- x:

  data frame object returned by `ms_summarize`

- level:

  analysis is on the "Gene" level or "Peptide" level

- alpha:

  significance level

- fc:

  minimum absolute fold change

- path:

  file path to save result object

## Value

A data frame showing the top variables. If `level = "Gene"`, the return
value is a 4 column data frame, showing the Gene, Accession, BH adjusted
omnibus p-value, and absolute fold change columns from `x`. If
`level = "Peptide"`, the return value is the same except the Gene and
Accession columns are replaced with the AGDSM column from `x`.

## Details

The input `x` is the result matrix returned by `ms_summarize`. We want
to filter `x` such that only interesting variables remain: i.e. those
that show statistical significance in an overall test and a
scientifically relevant effect size. Variables of interest are
summarized either on the gene level or peptide level.

By default, the level of statistical significance is set to 5\\
Benjamini-Hochberg adjusted omnibus test p-value. The minimum absolute
fold change for determining scientific relevance is 1.25 by default.
These default values can be modified for different studies or projects,
but offer a general measure of validity for users to start with.

## Note

The fold change criterion only needs to be satisfied for *one*
comparison if the experiment has 3 or more sample groups. For example,
suppose we have 1 control group and treatments A and B. We filter
variables on the fold change criterion where the absolute fold change is
greater than `fc` for *either* A vs. control or B vs. control.

## See also

Other Mass Spectrometry functions:
[`ms_condition()`](https://talhouklab.github.io/biostatUtil/reference/ms_condition.md),
[`ms_plot`](https://talhouklab.github.io/biostatUtil/reference/ms_plot.md),
[`ms_process()`](https://talhouklab.github.io/biostatUtil/reference/ms_process.md),
[`ms_summarize()`](https://talhouklab.github.io/biostatUtil/reference/ms_summarize.md)

## Author

Derek Chiu
