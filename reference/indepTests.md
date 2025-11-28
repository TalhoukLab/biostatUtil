# Tests for Independence in Contingency Tables

The Pearson's Chi-Squared test, likelihood ratio (G test) of
independence, Fisher's Exact test, and linear-by-linear association test
are performed on the data matrix.

## Usage

``` r
indepTests(x, digits = 3)
```

## Arguments

- x:

  an object of class `CrossTable` containing the contingency table

- digits:

  number of digits to round to

## Value

A table with method name, test statistic, degrees of freedom, and
p-value reported for each Chi-squared test.

## Details

A Pearson's Chi-Squared test Yate's Continuity Correction is applied in
the case of 2 by 2 tables.

## See also

[`descr::CrossTable()`](https://rdrr.io/pkg/descr/man/CrossTable.html)

## Author

Derek Chiu

## Examples

``` r
# Example from documentation of CrossTable
library(descr)
data(esoph, package = "datasets")
ct <- CrossTable(esoph$alcgp, esoph$agegp, expected = TRUE,
                 chisq = FALSE, prop.chisq = FALSE,
                 dnn = c("Alcohol consumption", "Tobacco consumption"))
indepTests(ct)
#>                                                      Test  Value   df P-Value
#> Pearson Chi-Square                     Pearson Chi-Square   <NA> <NA>    <NA>
#> Continuity Correction               Continuity Correction   <NA> <NA>    <NA>
#> Likelihood Ratio                         Likelihood Ratio   1.42   15       1
#> Fisher's Exact Test                   Fisher's Exact Test   <NA> <NA>    <NA>
#> Linear-by-Linear Association Linear-by-Linear Association -0.142    1   0.887
#> 6                                        N of Valid Cases     88             

# Better example
set.seed(1108)
A <- rbinom(100, 3, 0.2)
B <- rbinom(100, 4, 0.8)
ct <- CrossTable(A, B)
#> Warning: Chi-squared approximation may be incorrect
indepTests(ct)
#>                                                      Test Value   df P-Value
#> Pearson Chi-Square                     Pearson Chi-Square  <NA> <NA>    <NA>
#> Continuity Correction               Continuity Correction  <NA> <NA>    <NA>
#> Likelihood Ratio                         Likelihood Ratio 14.36    9    0.11
#> Fisher's Exact Test                   Fisher's Exact Test  <NA> <NA>    <NA>
#> Linear-by-Linear Association Linear-by-Linear Association 0.274    1   0.784
#> 6                                        N of Valid Cases   100             
```
