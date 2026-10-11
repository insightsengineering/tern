# Description of method used for proportion comparison

**\[stable\]**

This is an auxiliary function that describes the analysis in
[`s_proportion_diff()`](https://pharmaverse.github.io/tern/reference/prop_diff.md).

## Usage

``` r
d_proportion_diff(conf_level, method, long = FALSE, method_only = FALSE)
```

## Arguments

- conf_level:

  (`proportion`)\
  confidence level of the interval.

- method:

  (`string`)\
  the method used for the confidence interval estimation.

- long:

  (`flag`)\
  whether a long or a short (default) description is required.

- method_only:

  (`flag`)\
  whether to return only the method description, without the confidence
  interval part of the description. If `TRUE`, `conf_level` and `long`
  are ignored.

## Value

A `string` describing the analysis.

## See also

[`prop_diff()`](https://pharmaverse.github.io/tern/reference/prop_diff.md),
[`d_proportion()`](https://pharmaverse.github.io/tern/reference/d_proportion.md),
[`d_test_proportion_diff()`](https://pharmaverse.github.io/tern/reference/d_test_proportion_diff.md)

## Examples

``` r
d_proportion_diff(0.95, "cmh_sato")
#> [1] "95% CI (CMH, Sato variance estimator)"
d_proportion_diff(0.95, "cmh_sato", long = TRUE)
#> [1] "95% CI for adjusted difference (CMH, Sato variance estimator)"
d_proportion_diff(0.95, "cmh_sato", method_only = TRUE)
#> [1] "CMH, Sato variance estimator"
```
