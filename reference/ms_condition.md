# Sample count condition for MS analyses

The number of non-missing samples per treatment group determine whether
it can be used in subsequent analyses.

## Usage

``` r
ms_condition(data, treatment, control = NULL, condition = c("VC", "MRC", "MC"))
```

## Arguments

- data:

  The data matrix with rows as observations, columns as variables

- treatment:

  A character vector indicating how different treatment groups are
  coded. Regular expression is used on the column names to find all
  relevant treatment variables.

- control:

  Optionally specify a character string indicating the control group if
  it exists. Regular expression is used on the column names to find all
  relevant control variables.

- condition:

  Either one of "VC" (default), "MRC", or "MC". See details.

## Value

A logical vector of length equal to the number of rows in `data`,
indicating which observations are kept and which are to be removed.

## Details

The Variance Condition (VC) (default) requires at least two non-missing
samples in each of the treatment groups. A rudimentary estimate of
variance is possible when this phenomenon exists. The Minimum Reasonable
Condition (MRC) requires all treatment groups to have at least one
non-missing sample and one treatment group to have at least two
non-missing samples. Equal variance assumption is used in the case of
MRC. Finally, the Minimum Condition (MC) requires at least one
non-missing sample in each of the treatment groups. MC is not generally
suggested in practice.

Additionally, if there are controls, they are handled by MC.

## See also

Other Mass Spectrometry functions:
[`ms_plot`](https://talhouklab.github.io/biostatUtil/reference/ms_plot.md),
[`ms_process()`](https://talhouklab.github.io/biostatUtil/reference/ms_process.md),
[`ms_summarize()`](https://talhouklab.github.io/biostatUtil/reference/ms_summarize.md),
[`ms_top()`](https://talhouklab.github.io/biostatUtil/reference/ms_top.md)

## Author

Derek Chiu

## Examples

``` r
# Example data matrix with some missing sample values
g <- c("X5", "X19")
r <- c("1", "2", "3")
col.nm <- sort(apply(expand.grid(g, r), 1, paste, collapse = "_"))
A <- matrix(c(seq_len(36)), ncol = 6, byrow = FALSE, dimnames = list(NULL, col.nm))
A[c(1, 21, 26, 7, 17, 8, 23, 4, 16, 19, 5, 25, 12, 18, 24)] <- NA

# Two treatment experimental design
ms_condition(A, treatment = c("X19", "X5"), condition = "VC")
#> [1] FALSE  TRUE  TRUE FALSE FALSE FALSE
ms_condition(A, treatment = c("X19", "X5"), condition = "MRC")
#> [1] FALSE  TRUE  TRUE  TRUE  TRUE  TRUE
ms_condition(A, treatment = c("X19", "X5"), condition = "MC")
#> [1] TRUE TRUE TRUE TRUE TRUE TRUE

# Control ends in "_1", Treatments ends in "_2", "_3"
ms_condition(A, treatment = c("_2", "_3"), control = "_1", condition = "VC")
#> [1] FALSE FALSE  TRUE FALSE FALSE FALSE
ms_condition(A, treatment = c("_2", "_3"), control = "_1", condition = "MRC")
#> [1] FALSE FALSE  TRUE  TRUE FALSE FALSE
ms_condition(A, treatment = c("_2", "_3"), control = "_1", condition = "MC")
#> [1] FALSE FALSE  TRUE  TRUE FALSE  TRUE
```
