# Description of the proportion summary

**\[stable\]**

This is a helper function that describes the analysis in
[`s_proportion()`](https://pharmaverse.github.io/tern/reference/estimate_proportion.md).

## Usage

``` r
d_proportion(conf_level, method, long = FALSE, method_only = FALSE)
```

## Arguments

- conf_level:

  (`proportion`)\
  confidence level of the interval.

- method:

  (`string`)\
  the method used to construct the confidence interval for proportion of
  successful outcomes; one of `waldcc`, `wald`, `clopper-pearson`,
  `wilson`, `wilsonc`, `strat_wilson`, `strat_wilsonc`, `agresti-coull`
  or `jeffreys`.

- long:

  (`flag`)\
  whether a long or a short (default) description is required.

- method_only:

  (`flag`)\
  whether to return only the method description, without the confidence
  interval part of the description. If `TRUE`, `conf_level` and `long`
  are ignored.

## Value

String describing the analysis.

## See also

[`d_proportion_diff()`](https://pharmaverse.github.io/tern/reference/d_proportion_diff.md),
[`d_test_proportion_diff()`](https://pharmaverse.github.io/tern/reference/d_test_proportion_diff.md)

## Examples

``` r
d_proportion(0.95, "wald")
#> [1] "95% CI (Wald, without correction)"
d_proportion(0.95, "wald", long = TRUE)
#> [1] "95% CI for Response Rates (Wald, without correction)"
d_proportion(0.95, "wald", method_only = TRUE)
#> [1] "Wald, without correction"
```
