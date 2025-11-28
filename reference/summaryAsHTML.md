# Summary table in HTML format

Generate summary table as an HTML table

## Usage

``` r
summaryAsHTML(d)
```

## Arguments

- d:

  assume `d` is an array of numbers

## Value

summary table with annotated HTML code

## Author

Samuel Leung, Derek Chiu

## Examples

``` r
library(htmlTable)
set.seed(1)
x <- rnorm(100)
htmlTable(summaryAsHTML(x))
#> <table class='gmisc_table' style='border-collapse: collapse; margin-top: 1em; margin-bottom: 1em;'  id='table_6'>
#> <tbody>
#> <tr style='border-top: 2px solid grey;'>
#> <td style='border-top: 2px solid grey; border-bottom: 2px solid grey; text-align: center;'><table>
#>   <tr><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Min.</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">1st Qu.</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Median</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Mean</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">3rd Qu.</th><th style="border-bottom: 1px solid grey; border-top: 4px double grey; text-align: center; padding-right:10px; padding-right:10px;">Max.</th></tr>
#>   <tr><td>-2.2146998871775</td><td>-0.494242549079378</td><td>0.113909160788544</td><td>0.108887366914655</td><td>0.691545365689267</td><td>2.40161776050478</td></tr>
#> </table></td>
#> </tr>
#> </tbody>
#> </table>
```
