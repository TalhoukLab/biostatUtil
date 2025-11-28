# Extract results for executive summary

Extract statistical test results for Executive Summary section at
beginning of an RSF.

## Usage

``` r
extract_km(data, var, outcome, args, digits = 3)

extract_cox(x, var, outcome)
```

## Arguments

- data:

  data frame with clinical variables and outcomes

- var:

  variable name

- outcome:

  survival outcome. One of "os", "dss", "pfs".

- args:

  list of arguments for survival analyses

- digits:

  number of digits to round p-value

- x:

  an object from result of
  [`doCoxphGeneric()`](https://talhouklab.github.io/biostatUtil/reference/doCoxph.md)
  or
  [`doCoxphMultivariable()`](https://talhouklab.github.io/biostatUtil/reference/doCoxph.md)

## Value

Extracted result that can be used in the form of inline text as part of
the Executive Summary.

## Details

- `extract_km()` extracts the Kaplan-Meier Log Rank test p-value for a
  specified variable

- `extract_cox()` extracts Cox proportional hazards model hazard ratios
  with confidence intervals and LRT p-values for a specified variable

## Author

Derek Chiu
