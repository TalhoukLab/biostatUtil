# Build cutpoint variables

Transforms an ordinal variable into anywhere from two to five groups for
cutpoint analysis.

## Usage

``` r
build_cuts(x, n = c("b", "t", "qd", "qn"), var.prefix = NULL, list = FALSE)
```

## Arguments

- x:

  vector to split by certain cutpoints

- n:

  either "b" for binarization (2 groups), "t" for trinarization (3
  groups), "q" for quads (4 groups), or "qn" for quints (5 groups)

- var.prefix:

  variable name prefix

- list:

  if `TRUE`, the variables are returned as a list.

## Value

By default, a tibble of cutpoint variables built from a categorical
biomarker. The number of columns correspond to all the ways the
biomarker could be cut into `n` bins. Each column name starts with a
"b", "t", "qd", or "qn" for "binarization", "trinarization", "quads", or
"quints", respectively, with the levels being compared separated by "v".
If `list = FALSE`, each cutpoint variable is an element of a list.

## Author

Derek Chiu

## Examples

``` r
set.seed(1108)
x <- sample(0:4, size = 1000, replace = TRUE)
build_cuts(x, n = "b")
#> # A tibble: 1,000 × 4
#>    b0v1234 b01v234 b012v34 b0123v4
#>    <fct>   <fct>   <fct>   <fct>  
#>  1 [1,4]   [2,4]   [0,3)   [0,4)  
#>  2 [1,4]   [2,4]   [0,3)   [0,4)  
#>  3 [1,4]   [2,4]   [0,3)   [0,4)  
#>  4 [1,4]   [2,4]   [0,3)   [0,4)  
#>  5 0       [0,2)   [0,3)   [0,4)  
#>  6 [1,4]   [2,4]   [3,4]   [0,4)  
#>  7 [1,4]   [2,4]   [0,3)   [0,4)  
#>  8 [1,4]   [0,2)   [0,3)   [0,4)  
#>  9 [1,4]   [0,2)   [0,3)   [0,4)  
#> 10 [1,4]   [2,4]   [3,4]   4      
#> # ℹ 990 more rows
build_cuts(x, n = "t")
#> # A tibble: 1,000 × 6
#>    t0v1v234 t0v12v34 t0v123v4 t01v2v34 t01v23v4 t012v3v4
#>    <fct>    <fct>    <fct>    <fct>    <fct>    <fct>   
#>  1 [2,4]    [1,3)    [1,4)    2        [2,4)    [0,3)   
#>  2 [2,4]    [1,3)    [1,4)    2        [2,4)    [0,3)   
#>  3 [2,4]    [1,3)    [1,4)    2        [2,4)    [0,3)   
#>  4 [2,4]    [1,3)    [1,4)    2        [2,4)    [0,3)   
#>  5 0        0        0        [0,2)    [0,2)    [0,3)   
#>  6 [2,4]    [3,4]    [1,4)    [3,4]    [2,4)    3       
#>  7 [2,4]    [1,3)    [1,4)    2        [2,4)    [0,3)   
#>  8 1        [1,3)    [1,4)    [0,2)    [0,2)    [0,3)   
#>  9 1        [1,3)    [1,4)    [0,2)    [0,2)    [0,3)   
#> 10 [2,4]    [3,4]    4        [3,4]    4        4       
#> # ℹ 990 more rows
build_cuts(x, n = "t", var.prefix = "PHGDH")
#> # A tibble: 1,000 × 6
#>    PHGDH_t0v1v234 PHGDH_t0v12v34 PHGDH_t0v123v4 PHGDH_t01v2v34 PHGDH_t01v23v4
#>    <fct>          <fct>          <fct>          <fct>          <fct>         
#>  1 [2,4]          [1,3)          [1,4)          2              [2,4)         
#>  2 [2,4]          [1,3)          [1,4)          2              [2,4)         
#>  3 [2,4]          [1,3)          [1,4)          2              [2,4)         
#>  4 [2,4]          [1,3)          [1,4)          2              [2,4)         
#>  5 0              0              0              [0,2)          [0,2)         
#>  6 [2,4]          [3,4]          [1,4)          [3,4]          [2,4)         
#>  7 [2,4]          [1,3)          [1,4)          2              [2,4)         
#>  8 1              [1,3)          [1,4)          [0,2)          [0,2)         
#>  9 1              [1,3)          [1,4)          [0,2)          [0,2)         
#> 10 [2,4]          [3,4]          4              [3,4]          4             
#> # ℹ 990 more rows
#> # ℹ 1 more variable: PHGDH_t012v3v4 <fct>
str(build_cuts(x, n = "qd", list = TRUE))
#> List of 4
#>  $ qd0v1v2v34: Factor w/ 4 levels "0","1","2","[3,4]": 3 3 3 3 1 4 3 2 2 4 ...
#>  $ qd0v1v23v4: Factor w/ 4 levels "0","1","[2,4)",..: 3 3 3 3 1 3 3 2 2 4 ...
#>  $ qd0v12v3v4: Factor w/ 4 levels "0","[1,3)","3",..: 2 2 2 2 1 3 2 2 2 4 ...
#>  $ qd01v2v3v4: Factor w/ 4 levels "[0,2)","2","3",..: 2 2 2 2 1 3 2 1 1 4 ...
```
