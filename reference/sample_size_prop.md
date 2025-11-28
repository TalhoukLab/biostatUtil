# Sample Size for Multiple Proportions in One-Way Design

Calculates power and sample size in a one-way design for differences
among response proportions of those groups.

## Usage

``` r
sample_size_prop(props, power = NULL, n = NULL, alpha = 0.05)
```

## Arguments

- props:

  response proportion in each group

- power:

  desired power

- n:

  sample size in each group

- alpha:

  significance level

## Details

Specify exactly one of `power` or `n` in order to determine the value of
the other unspecified parameter.

## References

<https://www.ncss.com/wp-content/themes/ncss/pdf/Procedures/PASS/Tests_for_Multiple_Proportions_in_a_One-Way_Design.pdf>

## Examples

``` r
## Examples from PASS Sample Size Software reference
# Example 1
sample_size_prop(props = c(0.4, 0.2, 0.2), n = 20)
#> $power
#> [1] 0.286702
#> 
#> $V
#> [1] 0.1482442
#> 
sample_size_prop(props = c(0.4, 0.2, 0.2), n = 40)
#> $power
#> [1] 0.5265619
#> 
#> $V
#> [1] 0.1482442
#> 
sample_size_prop(props = c(0.4, 0.2, 0.2), n = 60)
#> $power
#> [1] 0.7124353
#> 
#> $V
#> [1] 0.1482442
#> 
sample_size_prop(props = c(0.4, 0.2, 0.2), n = 80)
#> $power
#> [1] 0.8367012
#> 
#> $V
#> [1] 0.1482442
#> 
sample_size_prop(props = c(0.4, 0.2, 0.2), n = 100)
#> $power
#> [1] 0.9120672
#> 
#> $V
#> [1] 0.1482442
#> 

# Example 2
sample_size_prop(props = c(0.4, 0.2, 0.2), power = 0.8)
#> $Ng
#> [1] 74
#> 
#> $N
#> [1] 222
#> 
#> $V
#> [1] 0.1473083
#> 
sample_size_prop(props = c(0.4, 0.2, 0.2), power = 0.9)
#> $Ng
#> [1] 96
#> 
#> $N
#> [1] 288
#> 
#> $V
#> [1] 0.1482182
#> 

# Example 3
sample_size_prop(power = 0.9, props = c(0.4, 0.1, 0.1))
#> $Ng
#> [1] 36
#> 
#> $N
#> [1] 108
#> 
#> $V
#> [1] 0.2420393
#> 
sample_size_prop(power = 0.9001, props = c(0.4, 0.2, 0.2))
#> $Ng
#> [1] 96
#> 
#> $N
#> [1] 288
#> 
#> $V
#> [1] 0.1482426
#> 
sample_size_prop(power = 0.9004, props = c(0.4, 0.3, 0.3))
#> $Ng
#> [1] 428
#> 
#> $N
#> [1] 1284
#> 
#> $V
#> [1] 0.07024275
#> 
sample_size_prop(power = 0.9038, props = c(0.4, 0.3, 0.1))
#> $Ng
#> [1] 49
#> 
#> $N
#> [1] 147
#> 
#> $V
#> [1] 0.208778
#> 

# Example 4
sample_size_prop(props = c(0.475, 0.2, 0.2, 0.2), n = 25)
#> $power
#> [1] 0.5721433
#> 
#> $V
#> [1] 0.149968
#> 
```
