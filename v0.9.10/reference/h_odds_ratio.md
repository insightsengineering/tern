# Helper functions for odds ratio estimation

**\[stable\]**

Functions to calculate odds ratios in
[`estimate_odds_ratio()`](https://insightsengineering.github.io/tern/reference/odds_ratio.md).

## Usage

``` r
or_glm(data, conf_level)

or_clogit(data, conf_level, method = "exact")
```

## Arguments

- data:

  (`data.frame`)  
  data frame containing at least the variables `rsp` and `grp`, and
  optionally `strata` for `or_clogit()`.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- method:

  (`string`)  
  whether to use the correct (`"exact"`) calculation in the conditional
  likelihood or one of the approximations. See
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html)
  for details.

## Value

A named `list` of elements `or_ci` and `n_tot`.

## Functions

- `or_glm()`: Estimates the odds ratio based on
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html). Note that there
  must be exactly 2 groups in `data` as specified by the `grp` variable.

- `or_clogit()`: Estimates the odds ratio based on
  [`survival::clogit()`](https://rdrr.io/pkg/survival/man/clogit.html).
  This is done for the whole data set including all groups, since the
  results are not the same as when doing pairwise comparisons between
  the groups.

## See also

[odds_ratio](https://insightsengineering.github.io/tern/reference/odds_ratio.md)

## Examples

``` r
# Data with 2 groups.
data <- data.frame(
  rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1)),
  grp = letters[c(1, 1, 1, 2, 2, 2, 1, 2)],
  strata = letters[c(1, 2, 1, 2, 2, 2, 1, 2)],
  stringsAsFactors = TRUE
)

# Odds ratio based on glm.
or_glm(data, conf_level = 0.95)
#> $or_ci
#>        est        lcl        ucl 
#> 0.33333333 0.01669735 6.65441589 
#> 
#> $n_tot
#> n_tot 
#>     8 
#> 

# Data with 3 groups.
data <- data.frame(
  rsp = as.logical(c(1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0)),
  grp = letters[c(1, 1, 1, 2, 2, 2, 3, 3, 3, 3, 1, 1, 1, 2, 2, 2, 3, 3, 3, 3)],
  strata = LETTERS[c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2)],
  stringsAsFactors = TRUE
)

# Odds ratio based on stratified estimation by conditional logistic regression.
or_clogit(data, conf_level = 0.95)
#> $or_ci
#> $or_ci$b
#>        est        lcl        ucl 
#> 0.28814553 0.02981009 2.78522598 
#> 
#> $or_ci$c
#>       est       lcl       ucl 
#> 0.5367919 0.0673365 4.2791881 
#> 
#> 
#> $n_tot
#> n_tot 
#>    20 
#> 
```
