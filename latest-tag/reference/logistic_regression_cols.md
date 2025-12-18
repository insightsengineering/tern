# Logistic regression multivariate column layout function

**\[stable\]**

Layout-creating function which creates a multivariate column layout
summarizing logistic regression results. This function is a wrapper for
[`rtables::split_cols_by_multivar()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by_multivar.html).

## Usage

``` r
logistic_regression_cols(lyt, conf_level = 0.95)
```

## Arguments

- lyt:

  (`PreDataTableLayouts`)  
  layout that analyses will be added to.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

## Value

A layout object suitable for passing to further layouting functions.
Adding this function to an `rtable` layout will split the table into
columns corresponding to statistics `df`, `estimate`, `std_error`,
`odds_ratio`, `ci`, and `pvalue`.
