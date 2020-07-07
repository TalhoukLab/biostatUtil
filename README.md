
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biostatUtil

The goal of biostatUtil is to provide utility functions for date
formatting, survival analysis, confusion matrices, contingency tables,
and data visualization.

## Installation

You can install biostatUtil from GitHub with:

``` r
remotes::install_github("TalhoukLab/biostatUtil")
```

Note that there are a few administrative tasks to perform to ensure the
package can still be installed when the R version is updated.

1.  Run `remotes::install_deps()` to install all dependencies (these are
    packages listed in the `Imports` field of `DESCRIPTION`)
2.  Install remaining Bioconductor dependencies with
    `BiocManager::install()`
3.  Run `devtools::check()` to see which packages to install from
    `Suggests` field of `DESCRIPTION`
4.  Open Terminal and run `R CMD javareconf` to reconfigure Java paths
    and other configurations. Note that Java v1.8.0 is required for java
    dependencies to successfully load when `biostatUtil` is attached.
