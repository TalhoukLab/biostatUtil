# Make Kaplan-Meier plots

Make Kaplan-Meier plots

## Usage

``` r
doKMPlots(
  input.d,
  time,
  status,
  var.name,
  var.description,
  surv.type = c("os", "dss", "pfs", "aefs"),
  shading.colors = c("blue2", "red2", "deepskyblue", "indianred3"),
  main = NULL,
  line.name = NULL,
  line.pattern = NULL,
  legend = FALSE,
  cox.ref.group = NULL,
  use.firth = -1,
  CI = TRUE,
  bold_pval = FALSE,
  sig.level = 0.05,
  HR = TRUE,
  show.risk = TRUE,
  km.plot.ref.group = "single",
  single.test.type = "logrank",
  use.ggkm = FALSE,
  ...
)
```

## Arguments

- input.d:

  `data.frame` containing data

- time:

  follow up time

- status:

  status indicator

- var.name:

  name of variable to make Kaplan-Meier plots on

- var.description:

  description for `var.name`

- surv.type:

  survival outcome. Either "os", "dss", "pfs", or "aefs".

- shading.colors:

  colors for survival curves

- main:

  plot title

- line.name:

  names for each survival curve

- line.pattern:

  line type for survival curves

- legend:

  logical; if `TRUE`, the legend is overlaid on the graph (instead of on
  the side).

- cox.ref.group:

  specify reference group for cox model i.e. hazard ratio(s)

- use.firth:

  Whether to use Firth's correction for plotting the curves

- CI:

  logical; if `TRUE`, will plot confidence bands

- bold_pval:

  logical; if `TRUE`, p-values are bolded if statistically significant
  at `sig.level`

- sig.level:

  significance level; default 0.05

- HR:

  logical; if `TRUE`, will show hazard ratios

- show.risk:

  logical; if `TRUE`, will show the number of people at risk at each
  time of death beneath the plot

- km.plot.ref.group:

  specify KM plot reference group; "single" means a lump log-rank
  statistic

- single.test.type:

  test to use for survival curves. Defaults to "logrank".

- use.ggkm:

  if `TRUE`, will use function `ggkm` for plotting

- ...:

  additional arguments to other functions and methods

## Value

A Kaplan-Meier plot for the specified survival outcome split on the
desired variable.

## Author

Samuel Leung, Derek Chiu
