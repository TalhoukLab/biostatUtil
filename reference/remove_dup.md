# Remove duplicates

Remove duplicates for specified columns of a data frame

## Usage

``` r
remove_dup(x, cols)
```

## Arguments

- x:

  data frame

- cols:

  character vector of column names from `x` to remove duplicates

## Value

A data frame with potentially fewer rows than `x` after duplicated
entries have been removed and repeated information has been collapsed.

## Details

In Mass Spec data, there are occasionally duplicated entries that need
to be removed before further analysis. Duplication is indicated by the
`Quan.Info` and `PSM.Ambiguity` columns. `remove_dup` removes duplicates
for certain columns, then collapses repeated information into a single
row.

This function is intended to be used after a call to
[`dplyr::group_by()`](https://dplyr.tidyverse.org/reference/group_by.html)
such that the removal of duplicates is performed within each group of
unique protein IDs (e.g. `Reporter.Quan.Result.ID`).

## Author

Derek Chiu
