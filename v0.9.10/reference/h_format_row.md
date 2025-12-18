# Helper function to format the optional `g_lineplot` table

**\[stable\]**

## Usage

``` r
h_format_row(x, format, labels = NULL)
```

## Arguments

- x:

  (named `list`)  
  list of numerical values to be formatted and optionally labeled.
  Elements of `x` must be `numeric` vectors.

- format:

  (named `character` or `NULL`)  
  format patterns for `x`. Names of the `format` must match the names of
  `x`. This parameter is passed directly to the
  [`rtables::format_rcell`](https://insightsengineering.github.io/rtables/latest-tag/reference/format_rcell.html)
  function through the `format` parameter.

- labels:

  (named `character` or `NULL`)  
  optional labels for `x`. Names of the `labels` must match the names of
  `x`. When a label is not specified for an element of `x`, then this
  function tries to use `label` or `names` (in this order) attribute of
  that element (depending on which one exists and it is not `NULL` or
  `NA` or `NaN`). If none of these attributes are attached to a given
  element of `x`, then the label is automatically generated.

## Value

A single row `data.frame` object.

## Examples

``` r
mean_ci <- c(48, 51)
x <- list(mean = 50, mean_ci = mean_ci)
format <- c(mean = "xx.x", mean_ci = "(xx.xx, xx.xx)")
labels <- c(mean = "My Mean")
h_format_row(x, format, labels)
#>   My Mean             V1
#> 1    50.0 (48.00, 51.00)

attr(mean_ci, "label") <- "Mean 95% CI"
x <- list(mean = 50, mean_ci = mean_ci)
h_format_row(x, format, labels)
#>   My Mean    Mean 95% CI
#> 1    50.0 (48.00, 51.00)
```
