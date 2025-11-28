# Plot detailed Kaplan-Meier curves

KM plots with details of event counts.

## Usage

``` r
plotKMDetail(
  input.data,
  surv.formula,
  main.text = "",
  xlab.text = "",
  ylab.text = "",
  line.name,
  line.color,
  line.pattern = NULL,
  line.width = NULL,
  show.test = "single",
  single.test.type = "logrank",
  round.digits.p.value = 4,
  obs.survyrs,
  ten.years.surv.95CI,
  event.count,
  legend.pos = "bottomleft",
  file.name = "no.file",
  file.width = 7,
  file.height = 7,
  grey.scale = FALSE,
  show.single.test.pos,
  ...
)
```

## Arguments

- input.data:

  input `data.frame`

- surv.formula:

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

- round.digits.p.value:

  number of digits for p-value

- obs.survyrs:

  show the observed survival years survival rate on KM plot

- ten.years.surv.95CI:

  show ten year survival 95% confidence interval

- event.count:

  show the number of events at each time point

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

- ...:

  additional arguments to `plot`

## References

<http://courses.nus.edu.sg/course/stacar/internet/st3242/handouts/notes2.pdf>

## See also

[`plotKM()`](https://talhouklab.github.io/biostatUtil/reference/plotKM.md)

## Author

Samuel Leung
