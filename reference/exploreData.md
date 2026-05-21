# Graphical Exploration of a Dataset

Function to provide a graphical exploration of a dataset will print
results to file.

## Usage

``` r
exploreData(datmat)
```

## Arguments

- datmat:

  the data frame (only categorical and numerical variables will be
  analysed)

## Author

Aline Talhouk

## Examples

``` r
mtcars$vs <- as.factor(mtcars$vs)
mtcars$am <- as.factor(mtcars$am)
exploreData(mtcars)
#> agg_record_2560cf93141 
#>                      2 
file.remove("DataSummary.pdf")
#> [1] TRUE
```
