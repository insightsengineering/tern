# Helper function for the estimation of stratified quantiles

**\[stable\]**

This function wraps the estimation of stratified percentiles when we
assume the approximation for large numbers. This is necessary only in
the case proportions for each strata are unequal.

## Usage

``` r
strata_normal_quantile(vars, weights, conf_level)
```

## Arguments

- vars:

  (`character`)  
  variable names for the primary analysis variable to be iterated over.

- weights:

  (`numeric` or `NULL`)  
  weights for each level of the strata. If `NULL`, they are estimated
  using the iterative algorithm proposed in Yan and Su (2010) that
  minimizes the weighted squared length of the confidence interval.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

## Value

Stratified quantile.

## See also

[`prop_strat_wilson()`](https://insightsengineering.github.io/tern/reference/h_proportions.md)

## Examples

``` r
strata_data <- table(data.frame(
  "f1" = sample(c(TRUE, FALSE), 100, TRUE),
  "f2" = sample(c("x", "y", "z"), 100, TRUE),
  stringsAsFactors = TRUE
))
ns <- colSums(strata_data)
ests <- strata_data["TRUE", ] / ns
vars <- ests * (1 - ests) / ns
weights <- rep(1 / length(ns), length(ns))

strata_normal_quantile(vars, weights, 0.95)
#> [1] 1.133649
```
