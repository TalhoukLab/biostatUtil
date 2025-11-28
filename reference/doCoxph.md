# Fit a Cox proportional hazards regression model

Fits `coxph` on all survival endpoints. `doCoxphGeneric` fits
univariable models for each variable in `var.names`, and
`doCoxphMultivariable` fits a multivariable model for all variables
together in `var.names`.

## Usage

``` r
doCoxphGeneric(
  input.d,
  var.names,
  var.descriptions,
  show.var.detail = FALSE,
  show.group.name.for.bin.var = FALSE,
  var.ref.groups = NULL,
  var.names.surv.time = c("os.yrs", "dss.yrs", "rfs.yrs"),
  var.names.surv.time2 = NULL,
  var.names.surv.status = c("os.sts", "dss.sts", "rfs.sts"),
  event.codes.surv = c("os.event", "dss.event", "rfs.event"),
  surv.descriptions = c("OS", "DSS", "PFS"),
  var.strata = NULL,
  missing.codes = c("N/A", "", "Unk"),
  use.firth = 1,
  firth.caption = FIRTH.CAPTION,
  add_log_hr = FALSE,
  stat.test = "waldtest",
  bold_pval = FALSE,
  sig.level = 0.05,
  round.digits.hr = 2,
  round.digits.p.value = 4,
  round.small = FALSE,
  scientific = FALSE,
  caption = NA,
  html.table.border = 0,
  banded.rows = FALSE,
  css.class.name.odd = "odd",
  css.class.name.even = "even",
  split.table = 300,
  format = c("long", "wide"),
  ...
)

doCoxphMultivariable(
  input.d,
  var.names,
  var.descriptions,
  show.var.detail = FALSE,
  show.group.name.for.bin.var = FALSE,
  var.ref.groups = NULL,
  var.names.surv.time = c("os.yrs", "dss.yrs", "rfs.yrs"),
  var.names.surv.time2 = NULL,
  var.names.surv.status = c("os.sts", "dss.sts", "rfs.sts"),
  event.codes.surv = c("os.event", "dss.event", "rfs.event"),
  surv.descriptions = c("OS", "DSS", "PFS"),
  var.strata = NULL,
  missing.codes = c("N/A", "", "Unk"),
  use.firth = 1,
  firth.caption = FIRTH.CAPTION,
  add_log_hr = FALSE,
  stat.test = "waldtest",
  bold_pval = FALSE,
  sig.level = 0.05,
  round.digits.hr = 2,
  round.digits.p.value = 4,
  round.small = FALSE,
  scientific = FALSE,
  caption = NA,
  html.table.border = 0,
  banded.rows = FALSE,
  css.class.name.odd = "odd",
  css.class.name.even = "even",
  split.table = 300,
  format = c("long", "wide"),
  ...
)
```

## Arguments

- input.d:

  The `data.frame` containing the data

- var.names:

  variables to include as predictors

- var.descriptions:

  vector of strings to describe the variables as they are to appear in
  the table

- show.var.detail:

  logical. If `TRUE`, details such as categories and the reference group
  for categorical variables are shown.

- show.group.name.for.bin.var:

  logical. If `TRUE`, the non-reference group name is shown beside the
  hazard ratio for dichotomized variables.

- var.ref.groups:

  a vector of reference groups. If `NULL`, assume all variables are
  binary/continuous. If an item in the vector is `NA`, assume that
  particular marker is binary or continuous (i.e., treat it as a numeric
  variable)

- var.names.surv.time:

  variable names of survival time

- var.names.surv.time2:

  optional variable names for interval ending times, used when survival
  is left-truncated. The interpretation of `var.names.surv.time` becomes
  interval starting times when this argument is not `NULL`.

- var.names.surv.status:

  variable names of survival status

- event.codes.surv:

  event coding of survival status variable

- surv.descriptions:

  names abbreviated survival endpoints in returned output

- var.strata:

  optional variable name to stratify by using
  [`survival::strata()`](https://rdrr.io/pkg/survival/man/strata.html)

- missing.codes:

  character strings of missing values used in `input.d`

- use.firth:

  percentage of censored cases before using Firth's method for Cox
  regression. If `use.firth = 1` (default), Firth is never used and if
  `use.firth = -1` Firth is always used.

- firth.caption:

  subscript in html table output indicating Firth was used

- add_log_hr:

  if `TRUE`, show the log hazard ratio

- stat.test:

  the overall model test to perform on the Cox regression model. Can be
  any of "waldtest", "logtest", or "sctest". If Firth is used, only
  "logtest" can be performed. Test p-values are never Firth corrected (
  per instruction from Aline 2015-04-14,15).

- bold_pval:

  logical; if `TRUE`, p-values are bolded if statistically significant
  at `sig.level`

- sig.level:

  significance level; default 0.05

- round.digits.hr:

  number of digits for hazard ratio

- round.digits.p.value:

  number of digits for p-value

- round.small:

  if `TRUE`, uses small number rounding via
  [`round_small()`](https://talhouklab.github.io/biostatUtil/reference/round_small.md)
  for p-values

- scientific:

  if `TRUE`, uses scientific notation when rounding

- caption:

  caption for returned object

- html.table.border:

  the border type to use for html tables

- banded.rows:

  logical. If `TRUE`, rows have alternating shading colour

- css.class.name.odd:

  Used to set the row colour for odd rows

- css.class.name.even:

  Used to set the row colour for even rows

- split.table:

  number of characters per row before splitting the table. Applies to
  the pandoc table output.

- format:

  Either a "long" or "wide" format for the `result.table.bamboo` result
  element

- ...:

  additional arguments to
  [`pander::pandoc.table.return()`](https://rdrr.io/pkg/pander/man/pandoc.table.return.html)

## Value

A list with the following elements

- `result.table`: a data frame with the Cox model results.

- `result.table.bamboo`: results transformed into a pandoc-ready format.
  Display using [`cat()`](https://rdrr.io/r/base/cat.html).

- `result.table.html`: results transformed into an HTML-ready format.
  Display using
  [`htmlTable::htmlTable()`](https://rdrr.io/pkg/htmlTable/man/htmlTable.html).

- `cox.stats`: present for `doCoxphMultivariable()` only. A list of
  additional Cox model output statistics.

## Details

Please note the following assumptions:

- Marker can be binary, continuous or categorical.

- Missing survival time/status variables are coded as `NA` (i.e. will
  only be checked by [`is.na()`](https://rdrr.io/r/base/NA.html)).

- Survival time/status variable name specified in the following order:
  "os", "dss", "rfs".

- Coding of survival status is binary only (i.e. cannot take survival
  status of \> 2 categories).

## Author

Samuel Leung, Aline Talhouk, Derek Chiu

## Examples

``` r
library(survival)
doCoxphGeneric(input.d = lung, var.names = "sex", var.descriptions = "Sex",
               var.names.surv.time = "time",
               var.names.surv.status = "status", event.codes.surv = "2",
               surv.descriptions = "OS", caption = "")
#> $result.table
#>        # of events / n Hazard Ratio (95% CI) P-value 
#> sex-OS "165 / 228"     "0.59 (0.42-0.82)"    "0.0015"
#> 
#> $result.table.bamboo
#> [1] "\n-------------------------------------------------------------\n &nbsp;    # of events / n   Hazard Ratio (95% CI)   P-value \n--------- ----------------- ----------------------- ---------\n **Sex**                                                     \n\n   OS         165 / 228        0.59 (0.42-0.82)      0.0015  \n-------------------------------------------------------------\n\n"
#> 
#> $result.table.html
#> [1] "<table border=0><caption style='display: table-caption; text-align: left;'></caption><tr><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;' colspan=2></th><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;'># of events / n</th><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;'>Hazard Ratio (95% CI)</th><th style='border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;'>P-value</th></tr><tr><th style='text-align: center; padding-right:10px; padding-right:10px;' rowspan=1>Sex</th><th style='text-align: center; padding-right:10px; padding-right:10px;'>OS</th><td>165 / 228</td><td>0.59 (0.42-0.82)</td><td>0.0015</td></tr></table>"
#> 
```
