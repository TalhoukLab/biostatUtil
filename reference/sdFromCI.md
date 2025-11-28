# Standard deviation of log hazard ratio

Compute the standard deviation of a log hazard ratio when only given its
repoted confidence limits.

## Usage

``` r
sdFromCI(HR, lower.limit, upper.limit, alpha = 0.05)
```

## Arguments

- HR:

  reported hazard ratio

- lower.limit:

  reported lower confidence limit

- upper.limit:

  reported upper confidence limit

- alpha:

  significance level

## Value

A list with elements

- sd:

  standard deviation of log HR

- pval:

  estimated p-value

## Details

Extract standard deviation from a confidence limit and obtain a
corresponding p-value, which can be compared to the reported p-value.
The null p-values are calculated from the Chi-squared 1 distribution.

## References

<http://www.ncbi.nlm.nih.gov/pmc/articles/PMC1920534/>

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
library(meta)
#> Loading required package: metadat
#> Loading 'meta' package (version 8.2-1).
#> Type 'help(meta)' for a brief overview.
studlab <- c("PORTEC", "Leuven", "TCGA", "Billingsley")
effects <- log(c(0.43, 0.18, 0.12, 0.37))
se_effects <- c(sdFromCI(0.43, 0.13, 1.37)$sd,
                sdFromCI(0.18, 0.01, 3.11)$sd,
                sdFromCI(0.12, 0.01, 2.11)$sd,
                sdFromCI(0.37, 0.09, 1.54)$sd)
metagen(effects, se_effects, studlab = studlab, sm = "HR", comb.fixed = TRUE)
#> Warning: Use argument 'common' instead of 'comb.fixed' (deprecated).
#> Number of studies: k = 4
#> 
#>                          HR           95%-CI     z p-value
#> Common effect model  0.3374 [0.1482; 0.7680] -2.59  0.0096
#> Random effects model 0.3374 [0.1482; 0.7680] -2.59  0.0096
#> 
#> Quantifying heterogeneity (with 95%-CIs):
#>  tau^2 = 0 [0.0000; 3.8485]; tau = 0 [0.0000; 1.9618]
#>  I^2 = 0.0% [0.0%; 84.7%]; H = 1.00 [1.00; 2.56]
#> 
#> Test of heterogeneity:
#>     Q d.f. p-value
#>  0.94    3  0.8166
#> 
#> Details of meta-analysis methods:
#> - Inverse variance method
#> - Restricted maximum-likelihood estimator for tau^2
#> - Q-Profile method for confidence interval of tau^2 and tau
#> - Calculation of I^2 based on Q
```
