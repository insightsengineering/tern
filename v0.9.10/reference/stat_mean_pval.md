# p-Value of the mean

**\[stable\]**

Convenient function for calculating the two-sided p-value of the mean.

## Usage

``` r
stat_mean_pval(x, na.rm = TRUE, n_min = 2, test_mean = 0)
```

## Arguments

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- na.rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- n_min:

  (`numeric(1)`)  
  a minimum number of non-missing `x` to estimate the p-value of the
  mean.

- test_mean:

  (`numeric(1)`)  
  mean value to test under the null hypothesis.

## Value

A p-value.

## Examples

``` r
stat_mean_pval(sample(10))
#>     p_value 
#> 0.000278196 

stat_mean_pval(rnorm(10), test_mean = 0.5)
#>   p_value 
#> 0.4815848 
```
