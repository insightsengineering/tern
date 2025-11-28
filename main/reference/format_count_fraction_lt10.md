# Format count and fraction with special case for count \< 10

**\[stable\]**

Formats a count together with fraction with special consideration when
count is less than 10.

## Usage

``` r
format_count_fraction_lt10(x, ...)
```

## Arguments

- x:

  (`numeric(2)`)  
  vector of length 2 with count and fraction, respectively.

- ...:

  not used. Required for `rtables` interface.

## Value

A string in the format `count (fraction %)`. If `count` is less than 10,
only `count` is printed.

## See also

Other formatting functions:
[`extreme_format`](https://insightsengineering.github.io/tern/reference/extreme_format.md),
[`format_auto()`](https://insightsengineering.github.io/tern/reference/format_auto.md),
[`format_count_fraction()`](https://insightsengineering.github.io/tern/reference/format_count_fraction.md),
[`format_count_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_count_fraction_fixed_dp.md),
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
format_count_fraction_lt10(x = c(275, 0.9673))
#> [1] "275 (96.7%)"
format_count_fraction_lt10(x = c(2, 0.6667))
#> [1] "2"
format_count_fraction_lt10(x = c(9, 1))
#> [1] "9"
```
