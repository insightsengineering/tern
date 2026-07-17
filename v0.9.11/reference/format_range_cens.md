# Format range with censoring indicators

**\[experimental\]**

Formats a survival time range where the minimum and/or maximum may be a
censored observation. A `+` suffix is appended to a bound when the
corresponding censoring flag is `TRUE`.

## Usage

``` r
format_range_cens(digits = 1L)
```

## Arguments

- digits:

  (`integer(1)`)\
  number of decimal places to display. Defaults to `1L`.

## Value

An `rtables` formatting function that takes a `numeric(4)` vector of the
form `c(min, max, lower_censored, upper_censored)`, where
`lower_censored` and `upper_censored` are `0`/`1` (or `FALSE`/`TRUE`)
flags, and returns a string in the format `"min to max"`, with `+`
appended to `min` and/or `max` when the corresponding censoring flag is
non-zero.

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
[`format_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_fraction_fixed_dp.md),
[`format_fraction_threshold()`](https://insightsengineering.github.io/tern/reference/format_fraction_threshold.md),
[`format_sigfig()`](https://insightsengineering.github.io/tern/reference/format_sigfig.md),
[`format_xx()`](https://insightsengineering.github.io/tern/reference/format_xx.md),
[`formatting_functions`](https://insightsengineering.github.io/tern/reference/formatting_functions.md)

## Examples

``` r
fmt <- format_range_cens(1L)
fmt(c(1.23, 9.87, 1, 0))
#> [1] "1.2+ to 9.9"
fmt(c(1.23, 9.87, 0, 0))
#> [1] "1.2 to 9.9"
```
