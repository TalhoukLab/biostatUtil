# Do interaction test with cox model likelihood ratio test only - two terms only

Only a LRT can be performed (2015-08-12). TODO: add penalized LRT?

## Usage

``` r
doInteractionCox(
  input.d,
  var.names,
  var.descriptions,
  show.var.detail = FALSE,
  show.group.name.for.bin.var = FALSE,
  var.ref.groups = NULL,
  var.names.surv.time = c("os.yrs", "dss.yrs", "rfs.yrs"),
  var.names.surv.status = c("os.sts", "dss.sts", "rfs.sts"),
  event.codes.surv = c("os.event", "dss.event", "rfs.event"),
  surv.descriptions = c("OS", "DSS", "PFS"),
  missing.codes = c("N/A", "", "Unk"),
  use.firth = 1,
  firth.caption = FIRTH.CAPTION,
  round.digits.p.value = 4,
  round.small = FALSE,
  scientific = FALSE,
  caption = NA,
  html.table.border = 0,
  banded.rows = FALSE,
  css.class.name.odd = "odd",
  css.class.name.even = "even",
  split.table = 300,
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

- var.names.surv.status:

  variable names of survival status

- event.codes.surv:

  event coding of survival status variable

- surv.descriptions:

  names abbreviated survival endpoints in returned output

- missing.codes:

  character strings of missing values used in `input.d`

- use.firth:

  percentage of censored cases before using Firth's method for Cox
  regression. If `use.firth = 1` (default), Firth is never used and if
  `use.firth = -1` Firth is always used.

- firth.caption:

  subscript in html table output indicating Firth was used

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

- ...:

  additional arguments to
  [`pander::pandoc.table.return()`](https://rdrr.io/pkg/pander/man/pandoc.table.return.html)

## Value

A list with the following elements

## Note

the order of variable names in var.names dictates when will the term
added in stepwise likelihood ratio test of nested models i.e. given
`var.names = c("A", "B", "C")`, likelihood ratio test of nested model
will be:

A A + B A + B + C

## Author

Samuel Leung
