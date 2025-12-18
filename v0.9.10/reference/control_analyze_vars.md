# Control function for descriptive statistics

**\[stable\]**

Sets a list of parameters for summaries of descriptive statistics.
Typically used internally to specify details for
[`s_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).
This function family is mainly used by
[`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).

## Usage

``` r
control_analyze_vars(
  conf_level = 0.95,
  quantiles = c(0.25, 0.75),
  quantile_type = 2,
  test_mean = 0
)
```

## Arguments

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- quantiles:

  (`numeric(2)`)  
  vector of length two to specify the quantiles to calculate.

- quantile_type:

  (`numeric(1)`)  
  number between 1 and 9 selecting quantile algorithms to be used.
  Default is set to 2 as this matches the default quantile algorithm in
  SAS `proc univariate` set by `QNTLDEF=5`. This differs from R's
  default. See more about `type` in
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html).

- test_mean:

  (`numeric(1)`)  
  number to test against the mean under the null hypothesis when
  calculating p-value.

## Value

A list of components with the same names as the arguments.
