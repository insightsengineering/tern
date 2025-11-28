# Helper function for `s_count_cumulative()`

**\[stable\]**

Helper function to calculate count and fraction of `x` values in the
lower or upper tail given a threshold.

## Usage

``` r
h_count_cumulative(
  x,
  threshold,
  lower_tail = TRUE,
  include_eq = TRUE,
  na_rm = TRUE,
  denom
)
```

## Arguments

- x:

  (`numeric`)  
  vector of numbers we want to analyze.

- threshold:

  (`numeric(1)`)  
  a cutoff value as threshold to count values of `x`.

- lower_tail:

  (`flag`)  
  whether to count lower tail, default is `TRUE`.

- include_eq:

  (`flag`)  
  whether to include value equal to the `threshold` in count, default is
  `TRUE`.

- na_rm:

  (`flag`)  
  whether `NA` values should be removed from `x` prior to analysis.

- denom:

  (`string`)  
  choice of denominator for proportion. Options are:

  - `n`: number of values in this row and column intersection.

  - `N_row`: total number of values in this row across columns.

  - `N_col`: total number of values in this column across rows.

## Value

A named vector with items:

- `count`: the count of values less than, less or equal to, greater
  than, or greater or equal to a threshold of user specification.

- `fraction`: the fraction of the count.

## See also

[count_cumulative](https://insightsengineering.github.io/tern/reference/count_cumulative.md)

## Examples

``` r
set.seed(1, kind = "Mersenne-Twister")
x <- c(sample(1:10, 10), NA)
.N_col <- length(x)

h_count_cumulative(x, 5, denom = .N_col)
#>     count  fraction 
#> 5.0000000 0.4545455 
h_count_cumulative(x, 5, lower_tail = FALSE, include_eq = FALSE, na_rm = FALSE, denom = .N_col)
#>     count  fraction 
#> 6.0000000 0.5454545 
h_count_cumulative(x, 0, lower_tail = FALSE, denom = .N_col)
#>      count   fraction 
#> 10.0000000  0.9090909 
h_count_cumulative(x, 100, lower_tail = FALSE, denom = .N_col)
#>    count fraction 
#>        0        0 
```
