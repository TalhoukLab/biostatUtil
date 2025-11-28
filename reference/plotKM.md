# Plot Kaplan-Meier curves

KM plots with basic annotations.

## Usage

``` r
plotKM(
  input.d,
  input.formula,
  main.text = "",
  xlab.text = "",
  ylab.text = "",
  line.name,
  line.color,
  line.pattern = NULL,
  line.width = NULL,
  show.test = "single",
  single.test.type = "logrank",
  digits = 3,
  obs.survyrs = 3,
  legend.pos = "bottomleft",
  file.name = "no.file",
  file.width = 7,
  file.height = 7,
  grey.scale = FALSE,
  show.single.test.pos = 0.1,
  xlabs = NULL,
  legend.xy = NULL,
  ylabs = NULL,
  timeby = NULL,
  ...
)
```

## Arguments

- input.d:

  input `data.frame`

- input.formula:

  survival formula to `Surv`

- main.text:

  plot title

- xlab.text:

  x-axis label

- ylab.text:

  y-axis label

- line.name:

  name of legend

- line.color:

  line colour of survival curves

- line.pattern:

  line pattern of survival curves

- line.width:

  line width of survival curves

- show.test:

  show single or the reference group value (for pairwise comparisons).
  If `"none"`, then no test is show.

- single.test.type:

  test to show if specified `show.test = "single"`. Possible choices are
  `"logrank"` (default), `"wilcoxon"`, `"taroneware"`, or `"all"`.

- digits:

  number of digits for p-value

- obs.survyrs:

  show the observed survival years survival rate on KM plot

- legend.pos:

  legend position keyword

- file.name:

  name of file to save plot to

- file.width:

  width of figure in saved file

- file.height:

  height of figure in saved file

- grey.scale:

  logical. If `TRUE`, the plot will be in grey scale.

- show.single.test.pos:

  position to show single test; defaults to 0.5 if `legend.pos = "top"`.
  Otherwise 0.1

- xlabs:

  dummy param to match ggkm so they don't get passed to ...

- legend.xy:

  dummy param to match ggkm so they don't get passed to ...

- ylabs:

  dummy param to match ggkm so they don't get passed to ...

- timeby:

  dummy param to match ggkm so they don't get passed to ...

- ...:

  additional arguments to `plotKMDetail`

## See also

[`plotKMDetail()`](https://talhouklab.github.io/biostatUtil/reference/plotKMDetail.md)

## Author

Samuel Leung
