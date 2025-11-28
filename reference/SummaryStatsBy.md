# Generate summary statistics

Splits the data by two variables, computing relevant statistics for each
variable.

## Usage

``` r
SummaryStatsBy(
  data,
  by1,
  by2,
  var.names,
  stats = c("mean", "sd", "median", "IQR", "range", "missing"),
  digits = 3,
  format = c("raw", "pandoc", "html", "long")
)
```

## Arguments

- data:

  The `data.frame` containing the data

- by1:

  character string of splitting variable 1

- by2:

  character string of splitting variable 2

- var.names:

  character vector of variables to compute statistics for

- stats:

  statistics to compute for continuous variables

- digits:

  number of digits to round to

- format:

  format to return the table in. Either "raw", "pandoc" (for Word and
  PDF), "html", or "long" format for graphing and data manipulation
  using raw values.

## Details

The `data` is split by two variables, `by1` and `by2`, and statistics
are computed for continuous variables. Statistics currently supported
include `mean, sd, median, IQR, range`, and the number of missing cases.
For factor variables, the counts, column and row percentages are shown
for each of the variable levels.

Note that marginal statistics are also shown for `by1`, so the order in
which you split `data` matters.

There are four print options for the output: `raw` gives the output as a
character matrix, `pandoc` gives a Pandoc-friendly output for Word and
PDF reports, `html` gives HTML supported output, and `long` is a tidy
version of `raw`.

## Author

Aline Talhouk, Derek Chiu

## Examples

``` r
mtcars$vs <- as.factor(mtcars$vs); mtcars$am <- as.factor(mtcars$am)
SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg", "vs",
"qsec", "am"))
#>          cyl=4, gear=3     cyl=4, gear=4         cyl=4, gear=5      
#> **mpg**  ""                ""                    ""                 
#> mean     "21.5 &#177; NA"  "26.925 &#177; 4.807" "28.2 &#177; 3.111"
#> median   "21.5"            "25.85"               "28.2"             
#> IQR      "0"               "8.1"                 "2.2"              
#> range    "21.5"            "21.4-33.9"           "26-30.4"          
#> missing  "0"               "0"                   "0"                
#> **vs**   ""                ""                    ""                 
#> 0        "0 (0%, 0%)"      "0 (0%, 0%)"          "1 (5.56%, 50%)"   
#> 1        "1 (7.14%, 100%)" "8 (57.14%, 100%)"    "1 (7.14%, 50%)"   
#> **qsec** ""                ""                    ""                 
#> mean     "20.01 &#177; NA" "19.613 &#177; 1.454" "16.8 &#177; 0.141"
#> median   "20.01"           "19.185"              "16.8"             
#> IQR      "0"               "1.317"               "0.1"              
#> range    "20.01"           "18.52-22.9"          "16.7-16.9"        
#> missing  "0"               "0"                   "0"                
#> **am**   ""                ""                    ""                 
#> 0        "1 (5.26%, 100%)" "2 (10.53%, 25%)"     "0 (0%, 0%)"       
#> 1        "0 (0%, 0%)"      "6 (46.15%, 75%)"     "2 (15.38%, 100%)" 
#>          cyl=4                 cyl=6, gear=3        cyl=6, gear=4       
#> **mpg**  ""                    ""                   ""                  
#> mean     "26.664 &#177; 4.51"  "19.75 &#177; 2.333" "19.75 &#177; 1.552"
#> median   "26"                  "19.75"              "20.1"              
#> IQR      "7.6"                 "1.65"               "2.15"              
#> range    "21.4-33.9"           "18.1-21.4"          "17.8-21"           
#> missing  "0"                   "0"                  "0"                 
#> **vs**   ""                    ""                   ""                  
#> 0        "1 (9.09%)"           "0 (0%, 0%)"         "2 (11.11%, 50%)"   
#> 1        "10 (90.91%)"         "2 (14.29%, 100%)"   "2 (14.29%, 50%)"   
#> **qsec** ""                    ""                   ""                  
#> mean     "19.137 &#177; 1.682" "19.83 &#177; 0.552" "17.67 &#177; 1.125"
#> median   "18.9"                "19.83"              "17.66"             
#> IQR      "1.39"                "0.39"               "1.57"              
#> range    "16.7-22.9"           "19.44-20.22"        "16.46-18.9"        
#> missing  "0"                   "0"                  "0"                 
#> **am**   ""                    ""                   ""                  
#> 0        "3 (27.27%)"          "2 (10.53%, 100%)"   "2 (10.53%, 50%)"   
#> 1        "8 (72.73%)"          "0 (0%, 0%)"         "2 (15.38%, 50%)"   
#>          cyl=6, gear=5     cyl=6                 cyl=8, gear=3        
#> **mpg**  ""                ""                    ""                   
#> mean     "19.7 &#177; NA"  "19.743 &#177; 1.454" "15.05 &#177; 2.774" 
#> median   "19.7"            "19.7"                "15.2"               
#> IQR      "0"               "2.35"                "2.575"              
#> range    "19.7"            "17.8-21.4"           "10.4-19.2"          
#> missing  "0"               "0"                   "0"                  
#> **vs**   ""                ""                    ""                   
#> 0        "1 (5.56%, 100%)" "3 (42.86%)"          "12 (66.67%, 100%)"  
#> 1        "0 (0%, 0%)"      "4 (57.14%)"          "0 (0%, 0%)"         
#> **qsec** ""                ""                    ""                   
#> mean     "15.5 &#177; NA"  "17.977 &#177; 1.707" "17.143 &#177; 0.802"
#> median   "15.5"            "18.3"                "17.35"              
#> IQR      "0"               "2.43"                "0.672"              
#> range    "15.5"            "15.5-20.22"          "15.41-18"           
#> missing  "0"               "0"                   "0"                  
#> **am**   ""                ""                    ""                   
#> 0        "0 (0%, 0%)"      "4 (57.14%)"          "12 (63.16%, 100%)"  
#> 1        "1 (7.69%, 100%)" "3 (42.86%)"          "0 (0%, 0%)"         
#>          cyl=8, gear=5        cyl=8                
#> **mpg**  ""                   ""                   
#> mean     "15.4 &#177; 0.566"  "15.1 &#177; 2.56"   
#> median   "15.4"               "15.2"               
#> IQR      "0.4"                "1.85"               
#> range    "15-15.8"            "10.4-19.2"          
#> missing  "0"                  "0"                  
#> **vs**   ""                   ""                   
#> 0        "2 (11.11%, 100%)"   "14 (100%)"          
#> 1        "0 (0%, 0%)"         "0 (0%)"             
#> **qsec** ""                   ""                   
#> mean     "14.55 &#177; 0.071" "16.772 &#177; 1.196"
#> median   "14.55"              "17.175"             
#> IQR      "0.05"               "1.457"              
#> range    "14.5-14.6"          "14.5-18"            
#> missing  "0"                  "0"                  
#> **am**   ""                   ""                   
#> 0        "0 (0%, 0%)"         "12 (85.71%)"        
#> 1        "2 (15.38%, 100%)"   "2 (14.29%)"         
SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("vs",
"qsec"))
#>          cyl=4, gear=3     cyl=4, gear=4         cyl=4, gear=5      
#> **vs**   ""                ""                    ""                 
#> 0        "0 (0%, 0%)"      "0 (0%, 0%)"          "1 (5.56%, 50%)"   
#> 1        "1 (7.14%, 100%)" "8 (57.14%, 100%)"    "1 (7.14%, 50%)"   
#> **qsec** ""                ""                    ""                 
#> mean     "20.01 &#177; NA" "19.613 &#177; 1.454" "16.8 &#177; 0.141"
#> median   "20.01"           "19.185"              "16.8"             
#> IQR      "0"               "1.317"               "0.1"              
#> range    "20.01"           "18.52-22.9"          "16.7-16.9"        
#> missing  "0"               "0"                   "0"                
#>          cyl=4                 cyl=6, gear=3        cyl=6, gear=4       
#> **vs**   ""                    ""                   ""                  
#> 0        "1 (9.09%)"           "0 (0%, 0%)"         "2 (11.11%, 50%)"   
#> 1        "10 (90.91%)"         "2 (14.29%, 100%)"   "2 (14.29%, 50%)"   
#> **qsec** ""                    ""                   ""                  
#> mean     "19.137 &#177; 1.682" "19.83 &#177; 0.552" "17.67 &#177; 1.125"
#> median   "18.9"                "19.83"              "17.66"             
#> IQR      "1.39"                "0.39"               "1.57"              
#> range    "16.7-22.9"           "19.44-20.22"        "16.46-18.9"        
#> missing  "0"                   "0"                  "0"                 
#>          cyl=6, gear=5     cyl=6                 cyl=8, gear=3        
#> **vs**   ""                ""                    ""                   
#> 0        "1 (5.56%, 100%)" "3 (42.86%)"          "12 (66.67%, 100%)"  
#> 1        "0 (0%, 0%)"      "4 (57.14%)"          "0 (0%, 0%)"         
#> **qsec** ""                ""                    ""                   
#> mean     "15.5 &#177; NA"  "17.977 &#177; 1.707" "17.143 &#177; 0.802"
#> median   "15.5"            "18.3"                "17.35"              
#> IQR      "0"               "2.43"                "0.672"              
#> range    "15.5"            "15.5-20.22"          "15.41-18"           
#> missing  "0"               "0"                   "0"                  
#>          cyl=8, gear=5        cyl=8                
#> **vs**   ""                   ""                   
#> 0        "2 (11.11%, 100%)"   "14 (100%)"          
#> 1        "0 (0%, 0%)"         "0 (0%)"             
#> **qsec** ""                   ""                   
#> mean     "14.55 &#177; 0.071" "16.772 &#177; 1.196"
#> median   "14.55"              "17.175"             
#> IQR      "0.05"               "1.457"              
#> range    "14.5-14.6"          "14.5-18"            
#> missing  "0"                  "0"                  
SummaryStatsBy(mtcars, by1 = "cyl", by2 = "gear", var.names = c("mpg"))
#>         cyl=4, gear=3    cyl=4, gear=4         cyl=4, gear=5      
#> **mpg** ""               ""                    ""                 
#> mean    "21.5 &#177; NA" "26.925 &#177; 4.807" "28.2 &#177; 3.111"
#> median  "21.5"           "25.85"               "28.2"             
#> IQR     "0"              "8.1"                 "2.2"              
#> range   "21.5"           "21.4-33.9"           "26-30.4"          
#> missing "0"              "0"                   "0"                
#>         cyl=4                cyl=6, gear=3        cyl=6, gear=4       
#> **mpg** ""                   ""                   ""                  
#> mean    "26.664 &#177; 4.51" "19.75 &#177; 2.333" "19.75 &#177; 1.552"
#> median  "26"                 "19.75"              "20.1"              
#> IQR     "7.6"                "1.65"               "2.15"              
#> range   "21.4-33.9"          "18.1-21.4"          "17.8-21"           
#> missing "0"                  "0"                  "0"                 
#>         cyl=6, gear=5    cyl=6                 cyl=8, gear=3       
#> **mpg** ""               ""                    ""                  
#> mean    "19.7 &#177; NA" "19.743 &#177; 1.454" "15.05 &#177; 2.774"
#> median  "19.7"           "19.7"                "15.2"              
#> IQR     "0"              "2.35"                "2.575"             
#> range   "19.7"           "17.8-21.4"           "10.4-19.2"         
#> missing "0"              "0"                   "0"                 
#>         cyl=8, gear=5       cyl=8             
#> **mpg** ""                  ""                
#> mean    "15.4 &#177; 0.566" "15.1 &#177; 2.56"
#> median  "15.4"              "15.2"            
#> IQR     "0.4"               "1.85"            
#> range   "15-15.8"           "10.4-19.2"       
#> missing "0"                 "0"               
```
