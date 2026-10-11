# Description of the difference test between two proportions

**\[stable\]**

This is an auxiliary function that describes the analysis in
`s_test_proportion_diff`.

## Usage

``` r
d_test_proportion_diff(
  method,
  alternative = c("two.sided", "less", "greater"),
  method_only = FALSE
)
```

## Arguments

- method:

  (`string`)\
  one of `chisq`, `cmh`, `cmh_sato`, `cmh_wh`, `fisher`, or `schouten`;
  specifies the test used to calculate the p-value.

- alternative:

  (`string`)\
  whether `two.sided`, or one-sided `less` or `greater` p-value should
  be displayed.

- method_only:

  (`flag`)\
  whether to return only the method description, including the
  alternative hypothesis specification, without the "p-value" part of
  the description.

## Value

A `string` describing the test from which the p-value is derived.

## See also

[`d_proportion()`](https://pharmaverse.github.io/tern/reference/d_proportion.md),
[`d_proportion_diff()`](https://pharmaverse.github.io/tern/reference/d_proportion_diff.md)

## Examples

``` r
d_test_proportion_diff("cmh_sato")
#> [1] "p-value (Cochran-Mantel-Haenszel Test with Sato Variance Estimator)"
d_test_proportion_diff("cmh_sato", alternative = "greater")
#> [1] "p-value (Cochran-Mantel-Haenszel Test with Sato Variance Estimator, 1-sided, direction greater)"
d_test_proportion_diff("cmh_sato", alternative = "greater", method_only = TRUE)
#> [1] "Cochran-Mantel-Haenszel Test with Sato Variance Estimator, 1-sided, direction greater"
```
