# Format fraction and percentage with fixed single decimal place

**\[stable\]**

Formats a fraction together with ratio in percent with fixed single
decimal place. Includes trailing zero in case of whole number
percentages to always keep one decimal place.

## Usage

``` r
format_fraction_fixed_dp(x, ...)
```

## Arguments

- x:

  (named `integer`)  
  vector with elements `num` and `denom`.

- ...:

  not used. Required for `rtables` interface.

## Value

A string in the format `num / denom (ratio %)`. If `num` is 0, the
format is `num / denom`.

## See also

Other formatting functions:
[`extreme_format`](https://insightsengineering.github.io/tern/reference/extreme_format.md),
[`format_auto()`](https://insightsengineering.github.io/tern/reference/format_auto.md),
[`format_count_fraction()`](https://insightsengineering.github.io/tern/reference/format_count_fraction.md),
[`format_count_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_count_fraction_fixed_dp.md),
[`format_count_fraction_lt10()`](https://insightsengineering.github.io/tern/reference/format_count_fraction_lt10.md),
[`format_extreme_values()`](https://insightsengineering.github.io/tern/reference/format_extreme_values.md),
[`format_extreme_values_ci()`](https://insightsengineering.github.io/tern/reference/format_extreme_values_ci.md),
[`format_fraction()`](https://insightsengineering.github.io/tern/reference/format_fraction.md),
[`format_fraction_threshold()`](https://insightsengineering.github.io/tern/reference/format_fraction_threshold.md),
[`format_sigfig()`](https://insightsengineering.github.io/tern/reference/format_sigfig.md),
[`format_xx()`](https://insightsengineering.github.io/tern/reference/format_xx.md),
[`formatting_functions`](https://insightsengineering.github.io/tern/reference/formatting_functions.md)

## Examples

``` r
format_fraction_fixed_dp(x = c(num = 1L, denom = 2L))
#> [1] "1/2 (50.0%)"
format_fraction_fixed_dp(x = c(num = 1L, denom = 4L))
#> [1] "1/4 (25.0%)"
format_fraction_fixed_dp(x = c(num = 0L, denom = 3L))
#> [1] "0/3"
```
