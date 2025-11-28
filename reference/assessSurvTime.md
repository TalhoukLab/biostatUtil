# Assess survival time

Given range of survival times and censoring status, provides different
time-related summary statistics

## Usage

``` r
assessSurvTime(T1, T2, status)
```

## Arguments

- T1:

  vector of start dates

- T2:

  vector of end dates

- status:

  logical;

## Value

A list with elements

- Otime:

  Observation time

- Stime:

  Censoring time

- Etime:

  Time to end of study

- KFT:

  Known Function Time

- RevKM:

  Reverse Kaplan-Meier Time

## Details

The observation time is defined as the median time in days between all
`T1` and `T2`. Censoring time is the median time in days between all
`T1` and `T2` for events only.

## Author

Samuel Leung

## Examples

``` r
set.seed(3)
starts <- seq(as.Date("2000/1/1"), as.Date("2003/1/1"), by = "quarter")
ends <-  seq(as.Date("2003/1/1"), as.Date("2006/1/1"), by = "quarter")
statuses <- sample(0:1, 13, replace = TRUE)
assessSurvTime(starts, ends, statuses)
#> $Otime
#> [1] 3
#> 
#> $Stime
#> [1] 3
#> 
#> $Etime
#> [1] 4.5
#> 
#> $KFT
#> [1] 3
#> 
#> $RevKM
#> [1] 3
#> 
```
