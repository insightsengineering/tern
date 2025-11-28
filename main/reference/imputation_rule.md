# Apply 1/3 or 1/2 imputation rule to data

**\[stable\]**

## Usage

``` r
imputation_rule(
  df,
  x_stats,
  stat,
  imp_rule,
  post = FALSE,
  avalcat_var = "AVALCAT1"
)
```

## Arguments

- df:

  (`data.frame`)  
  data set containing all analysis variables.

- x_stats:

  (named `list`)  
  a named list of statistics, typically the results of
  [`s_summary()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).

- stat:

  (`string`)  
  statistic to return the value/NA level of according to the imputation
  rule applied.

- imp_rule:

  (`string`)  
  imputation rule setting. Set to `"1/3"` to implement 1/3 imputation
  rule or `"1/2"` to implement 1/2 imputation rule.

- post:

  (`flag`)  
  whether the data corresponds to a post-dose time-point (defaults to
  `FALSE`). This parameter is only used when `imp_rule` is set to
  `"1/3"`.

- avalcat_var:

  (`string`)  
  name of variable that indicates whether a row in `df` corresponds to
  an analysis value in category `"BLQ"`, `"LTR"`, `"<PCLLOQ"`, or none
  of the above (defaults to `"AVALCAT1"`). Variable `avalcat_var` must
  be present in `df`.

## Value

A `list` containing statistic value (`val`) and NA level (`na_str`) that
should be displayed according to the specified imputation rule.

## See also

[`analyze_vars_in_cols()`](https://insightsengineering.github.io/tern/reference/analyze_vars_in_cols.md)
where this function can be implemented by setting the `imp_rule`
argument.

## Examples

``` r
set.seed(1)
df <- data.frame(
  AVAL = runif(50, 0, 1),
  AVALCAT1 = sample(c(1, "BLQ"), 50, replace = TRUE)
)
x_stats <- s_summary(df$AVAL)
imputation_rule(df, x_stats, "max", "1/3")
#> $val
#>       max 
#> 0.9919061 
#> 
#> $na_str
#> [1] "ND"
#> 
imputation_rule(df, x_stats, "geom_mean", "1/3")
#> $val
#> [1] NA
#> 
#> $na_str
#> [1] "NE"
#> 
imputation_rule(df, x_stats, "mean", "1/2")
#> $val
#> [1] NA
#> 
#> $na_str
#> [1] "ND"
#> 
```
