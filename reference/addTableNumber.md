# Add table number

Prepends a table counter label to a table caption.

## Usage

``` r
addTableNumber(caption, table.counter.str.default = "Table %s: ")
```

## Arguments

- caption:

  table caption

- table.counter.str.default:

  the default string preceding the table count

## Value

A caption with a table counter prepended to its text.

## Details

The table counter automatically increments based on the number of
existing tables that have already called using `addTableNumber`.

## Author

Samuel Leung

## Examples

``` r
addTableNumber("Overall Survival Hazard Ratios")
#> [1] "Table 1: Overall Survival Hazard Ratios"
addTableNumber("Correlation Heatmap", table.counter.str.default = "Figure %s:
")
#> [1] "Figure 2:\nCorrelation Heatmap"
```
