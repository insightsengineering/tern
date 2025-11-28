# Proportion difference and confidence interval

**\[stable\]**

Function for calculating the proportion (or risk) difference and
confidence interval between arm X (reference group) and arm Y. Risk
difference is calculated by subtracting cumulative incidence in arm Y
from cumulative incidence in arm X.

## Usage

``` r
stat_propdiff_ci(
  x,
  y,
  N_x,
  N_y,
  list_names = NULL,
  conf_level = 0.95,
  pct = TRUE
)
```

## Arguments

- x:

  (`list` of `integer`)  
  list of number of occurrences in arm X (reference group).

- y:

  (`list` of `integer`)  
  list of number of occurrences in arm Y. Must be of equal length to
  `x`.

- N_x:

  (`numeric(1)`)  
  total number of records in arm X.

- N_y:

  (`numeric(1)`)  
  total number of records in arm Y.

- list_names:

  (`character`)  
  names of each variable/level corresponding to pair of proportions in
  `x` and `y`. Must be of equal length to `x` and `y`.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- pct:

  (`flag`)  
  whether output should be returned as percentages. Defaults to `TRUE`.

## Value

List of proportion differences and CIs corresponding to each pair of
number of occurrences in `x` and `y`. Each list element consists of 3
statistics: proportion difference, CI lower bound, and CI upper bound.

## See also

Split function
[`add_riskdiff()`](https://insightsengineering.github.io/tern/reference/add_riskdiff.md)
which, when used as `split_fun` within
[`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
with `riskdiff` argument is set to `TRUE` in subsequent analyze
functions, adds a column containing proportion (risk) difference to an
`rtables` layout.

## Examples

``` r
stat_propdiff_ci(
  x = list(0.375), y = list(0.01), N_x = 5, N_y = 5, list_names = "x", conf_level = 0.9
)
#> $x
#> [1]   7.30000 -12.35184  26.95184
#> 

stat_propdiff_ci(
  x = list(0.5, 0.75, 1), y = list(0.25, 0.05, 0.5), N_x = 10, N_y = 20, pct = FALSE
)
#> [[1]]
#> [1]  0.0375000 -0.1060891  0.1810891
#> 
#> [[2]]
#> [1]  0.07250000 -0.09220915  0.23720915
#> 
#> [[3]]
#> [1]  0.0750000 -0.1231285  0.2731285
#> 
```
