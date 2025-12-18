# Format fraction with lower threshold

**\[stable\]**

Formats a fraction when the second element of the input `x` is the
fraction. It applies a lower threshold, below which it is just stated
that the fraction is smaller than that.

## Usage

``` r
format_fraction_threshold(threshold)
```

## Arguments

- threshold:

  (`proportion`)  
  lower threshold.

## Value

An `rtables` formatting function that takes numeric input `x` where the
second element is the fraction that is formatted. If the fraction is
above or equal to the threshold, then it is displayed in percentage. If
it is positive but below the threshold, it returns, e.g. "\<1" if the
threshold is `0.01`. If it is zero, then just "0" is returned.

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
[`format_sigfig()`](https://insightsengineering.github.io/tern/reference/format_sigfig.md),
[`format_xx()`](https://insightsengineering.github.io/tern/reference/format_xx.md),
[`formatting_functions`](https://insightsengineering.github.io/tern/reference/formatting_functions.md)

## Examples

``` r
format_fun <- format_fraction_threshold(0.05)
format_fun(x = c(20, 0.1))
#> [1] 10
format_fun(x = c(2, 0.01))
#> [1] "<5"
format_fun(x = c(0, 0))
#> [1] "0"
```
