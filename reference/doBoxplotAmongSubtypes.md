# Do a boxplot among subtypes

A stripchart (jitter plot) of a biomarker and some categorical subtype
is superimposed on top of the boxplot

## Usage

``` r
doBoxplotAmongSubtypes(
  input.d,
  data.description,
  biomarker.var.name,
  biomarker.name,
  subtype.var.name,
  subtype.name,
  pch = 4,
  jitter = 0.1,
  digits = 2,
  ...
)
```

## Arguments

- input.d:

  input data.frame

- data.description:

  boxplot title description

- biomarker.var.name:

  biomarker variable name in `input.d` to graph on

- biomarker.name:

  boxplot y-axis label

- subtype.var.name:

  subtype variable name in `input.d` to graph on

- subtype.name:

  boxplot x-axis label

- pch:

  stripchart plot style

- jitter:

  amount of jitter in stripchart

- digits:

  number of digits to round to

- ...:

  additional arguments to `boxplot`

## Value

a boxplot of biomarker values across different subtypes

## Note

Expects subtype variable to be a factor. Also expects biomarker and
subtype variable missing values to be `NA`

## Author

Samuel Leung

## Examples

``` r
doBoxplotAmongSubtypes(mtcars, "Boxplot of qsec vs. gear", "qsec", "QSEC",
"gear", "GEAR")
```
