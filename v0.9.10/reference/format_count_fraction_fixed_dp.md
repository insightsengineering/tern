# Format count and percentage with fixed single decimal place

**\[experimental\]**

Formats a count together with fraction with special consideration when
count is `0`.

## Usage

``` r
format_count_fraction_fixed_dp(x, ...)
```

## Arguments

- x:

  (`numeric(2)`)  
  vector of length 2 with count and fraction, respectively.

- ...:

  not used. Required for `rtables` interface.

## Value

A string in the format `count (fraction %)`. If `count` is 0, the format
is `0`.

## See also

Other formatting functions:
[`extreme_format`](https://insightsengineering.github.io/tern/reference/extreme_format.md),
[`format_auto()`](https://insightsengineering.github.io/tern/reference/format_auto.md),
[`format_count_fraction()`](https://insightsengineering.github.io/tern/reference/format_count_fraction.md),
[`format_count_fraction_lt10()`](https://insightsengineering.github.io/tern/reference/format_count_fraction_lt10.md),
[`format_extreme_values()`](https://insightsengineering.github.io/tern/reference/format_extreme_values.md),
[`format_extreme_values_ci()`](https://insightsengineering.github.io/tern/reference/format_extreme_values_ci.md),
[`format_fraction()`](https://insightsengineering.github.io/tern/reference/format_fraction.md),
[`format_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_fraction_fixed_dp.md),
[`format_fraction_threshold()`](https://insightsengineering.github.io/tern/reference/format_fraction_threshold.md),
[`format_sigfig()`](https://insightsengineering.github.io/tern/reference/format_sigfig.md),
[`format_xx()`](https://insightsengineering.github.io/tern/reference/format_xx.md),
[`formatting_functions`](https://insightsengineering.github.io/tern/reference/formatting_functions.md)

## Examples

``` r
format_count_fraction_fixed_dp(x = c(2, 0.6667))
#> [1] "2 (66.7%)"
format_count_fraction_fixed_dp(x = c(2, 0.5))
#> [1] "2 (50.0%)"
format_count_fraction_fixed_dp(x = c(0, 0))
#> [1] "0"
```
