# Format extreme values part of a confidence interval

**\[stable\]**

Formatting Function for extreme values part of a confidence interval.
Values are formatted as e.g. "(xx.xx, xx.xx)" if the number of `digits`
is 2.

## Usage

``` r
format_extreme_values_ci(digits = 2L)
```

## Arguments

- digits:

  (`integer(1)`)  
  number of decimal places to display.

## Value

An `rtables` formatting function that uses threshold `digits` to return
a formatted extreme values confidence interval.

## See also

Other formatting functions:
[`extreme_format`](https://insightsengineering.github.io/tern/reference/extreme_format.md),
[`format_auto()`](https://insightsengineering.github.io/tern/reference/format_auto.md),
[`format_count_fraction()`](https://insightsengineering.github.io/tern/reference/format_count_fraction.md),
[`format_count_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_count_fraction_fixed_dp.md),
[`format_count_fraction_lt10()`](https://insightsengineering.github.io/tern/reference/format_count_fraction_lt10.md),
[`format_extreme_values()`](https://insightsengineering.github.io/tern/reference/format_extreme_values.md),
[`format_fraction()`](https://insightsengineering.github.io/tern/reference/format_fraction.md),
[`format_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_fraction_fixed_dp.md),
[`format_fraction_threshold()`](https://insightsengineering.github.io/tern/reference/format_fraction_threshold.md),
[`format_sigfig()`](https://insightsengineering.github.io/tern/reference/format_sigfig.md),
[`format_xx()`](https://insightsengineering.github.io/tern/reference/format_xx.md),
[`formatting_functions`](https://insightsengineering.github.io/tern/reference/formatting_functions.md)

## Examples

``` r
format_fun <- format_extreme_values_ci(2L)
format_fun(x = c(0.127, Inf))
#> [1] "(0.13, >999.99)"
format_fun(x = c(0, 0.009))
#> [1] "(0.00, <0.01)"
```
