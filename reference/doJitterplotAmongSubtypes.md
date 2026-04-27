# Do a jitterplot among subtypes

Generates stripchart (jitter plot) of a biomarker split on a categorical
subtype.

## Usage

``` r
doJitterplotAmongSubtypes(
  input.d,
  data.description,
  biomarker.var.name,
  biomarker.name,
  subtype.var.name,
  subtype.name,
  pch = ".",
  jitter = 0.05,
  digits = 3,
  cex.axis = 0.9
)
```

## Arguments

- input.d:

  input `data.frame`

- data.description:

  title description

- biomarker.var.name:

  variable name of biomarker to plot

- biomarker.name:

  x-axis label for biomarker name

- subtype.var.name:

  variable name of subtype to separate biomarker

- subtype.name:

  y-axis label for subtype name

- pch:

  jitterplot dot type. For example, 20 = small dot, 16 = big dot.

- jitter:

  scalar indicating amount of jitter (spread of points)

- digits:

  number of digits to round to

- cex.axis:

  scalar indicating amount of scaling for axes

## Value

A jitterplot shown across different subtypes

## Details

Expects subtype variable to be a factor. Also, biomarker and subtype
variables have missing cases as `NA`.

## Author

Samuel Leung

## Examples

``` r
doJitterplotAmongSubtypes(mtcars, "Boxplot of qsec vs. vs", "qsec", "QSEC",
"vs", "VS")
```
