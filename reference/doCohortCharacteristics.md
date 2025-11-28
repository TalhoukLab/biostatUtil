# Generate cohort characteristics

Generate cohort characteristics

## Usage

``` r
doCohortCharacteristics(
  input.d,
  marker.name,
  marker.description,
  var.names,
  is.var.continuous,
  var.descriptions,
  marker.value.labels.tolower = TRUE,
  show.missing = TRUE,
  show.missing.continuous = TRUE,
  do.droplevels = TRUE,
  show.percent = "both",
  stat.tests = NULL,
  chisq.test.simulate.p.value = FALSE,
  stat.test.column.header = "association/correlation test",
  show.test.name = TRUE,
  round.digits.p.value = 4,
  num.boot = 1000,
  missing.codes.highlight = NULL,
  missing.codes = c("N/A", "", "Unk"),
  decimal = 0,
  caption = NA,
  html.table.border = 0,
  banded.rows = FALSE,
  css.class.name.odd = "odd",
  css.class.name.even = "even",
  custom.marker.labels = NULL,
  custom.total.label = NULL,
  split.table = 200,
  ...
)
```

## Arguments

- input.d:

  The `data.frame` containing the data

- marker.name:

  The variable that you want to split into different columns

- marker.description:

  The description for the variable(s) to split

- var.names:

  The variables that you want the statistics for

- is.var.continuous:

  Vector of length equal to the length of var.names with 1 indicating a
  continuous variable and 0 otherwise (this should be inferred in the
  function)

- var.descriptions:

  Vector of strings to describe the variables as they are to appear in
  the table

- marker.value.labels.tolower:

  Indicator as to whether to put marker value labels to lower case

- show.missing:

  an indicator to whether to show missing values

- show.missing.continuous:

  if set to `FALSE` and `show.missing == FALSE`, will not show the
  number of missing cases for continuous variables. Otherwise, it shows
  the number of missing for continuous variables even if
  `show.missing == FALSE`.

- do.droplevels:

  drop categories of unobserved levels set to `TRUE`

- show.percent:

  defaults to "both" which shows both rows and columns other possible
  values: "column", "row".

- stat.tests:

  statistical test to perform. `NULL` indicates do not do test for all
  variables, `NA` indicates do not do test for specified variable.
  Tests: chisq, fisher, ttest, wilcox, kendall, spearman, pearson,
  kruskal, confusionMarkerAsRef, confusionVarAsRef

- chisq.test.simulate.p.value:

  Whether to simulate p-value for chi-square test. this parameter is
  ignored if chi-square is not used. Default value=FALSE

- stat.test.column.header:

  The name to show on the header defaults to "association/correlation
  test"

- show.test.name:

  logical; if `TRUE` (default), the name of each test is prepended to
  the p-value

- round.digits.p.value:

  The number of digits to round the P values

- num.boot:

  the number of bootstrap samples for any bootstrap method that may be
  used

- missing.codes.highlight:

  default to `NULL` this indicates whether we wanted the missing values
  broken down down or lumped together.

- missing.codes:

  a vector to indicate how missing values are coded, default is
  `c("N/A", "", "Unk")`

- decimal:

  number of decimal places to show for aggregate numbers such as
  proportions or averages; default to 0

- caption:

  caption to use for the Table

- html.table.border:

  the border type to use for html tables

- banded.rows:

  If `TRUE`, rows have alternating shading colour

- css.class.name.odd:

  Used to set the row colour for odd rows

- css.class.name.even:

  Used to set the row colour for even rows

- custom.marker.labels:

  labels of marker to show; default `NULL` means using existing value
  label of the marker

- custom.total.label:

  label of the "Total" column; default `NULL` means show "Total"

- split.table:

  number of chars per row before table is split.

- ...:

  additional arguments to `pander`

## Value

A table with statistics reported for multiple variables, such as mean,
median, and range for continuous variables and proportions and
percentages for categorical variables. Relevant association and
correlation tests are performed as well.

## Author

Aline Talhouk

## Examples

``` r
dcc <- doCohortCharacteristics( input.d = mtcars, marker.name = "cyl",
marker.description = "cylinders", var.names = c("disp", "hp"),
var.descriptions = c("displacement", "horsepower"), is.var.continuous =
c(TRUE, TRUE), caption = "Some mtcars summaries")
htmlTable::htmlTable(dcc$result.table.html)
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;'  id='table_4'>
#> <tbody>
#> <tr style='border-top: 2px solid grey;'>
#> <td style='border-top: 2px solid grey; border-bottom: 2px solid grey; text-align: center;'><table border=0><caption style='display: table-caption; text-align: left;'>Some mtcars summaries</caption><tr><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;' colspan=2></th><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;'>total</th><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;'>cylinders 4</th><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;'>cylinders 6</th><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;'>cylinders 8</th></tr><tr><th style='text-align: left; padding-right:10px; padding-right:10px;' colspan=2>total</th><td>32 (100%)</td><td>11 (34%)</td><td>7 (22%)</td><td>14 (44%)</td><td></td></tr><tr><th style='text-align: left; padding-right:10px; padding-right:10px;' colspan=2>displacement</th><td></td><td></td><td></td><td></td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>mean (SD)</th><td>231 (124)</td><td>105 (27)</td><td>183 (42)</td><td>353 (68)</td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>median</th><td>196</td><td>108</td><td>168</td><td>350</td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>IQR</th><td>121 to 326</td><td> 79 to 121</td><td>160 to 196</td><td>302 to 390</td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>range</th><td> 71 to 472</td><td> 71 to 147</td><td>145 to 258</td><td>276 to 472</td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>missing</th><td>0</td><td>0</td><td>0</td><td>0</td></tr><tr><th style='text-align: left; padding-right:10px; padding-right:10px;' colspan=2>horsepower</th><td></td><td></td><td></td><td></td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>mean (SD)</th><td>147 (69)</td><td>83 (21)</td><td>122 (24)</td><td>209 (51)</td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>median</th><td>123</td><td>91</td><td>110</td><td>192</td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>IQR</th><td> 96 to 180</td><td>66 to 96</td><td>110 to 123</td><td>176 to 241</td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>range</th><td> 52 to 335</td><td> 52 to 113</td><td>105 to 175</td><td>150 to 335</td></tr><tr><th>&nbsp;&nbsp;&nbsp;&nbsp;</th><th style='text-align: left; padding-right:10px; padding-right:10px;'>missing</th><td>0</td><td>0</td><td>0</td><td>0</td></tr></table></td>
#> </tr>
#> </tbody>
#> </table>
```
