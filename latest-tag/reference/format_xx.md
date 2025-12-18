# Format XX as a formatting function

Translate a string where x and dots are interpreted as number place
holders, and others as formatting elements.

## Usage

``` r
format_xx(str)
```

## Arguments

- str:

  (`string`)  
  template.

## Value

An `rtables` formatting function.

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
[`formatting_functions`](https://insightsengineering.github.io/tern/reference/formatting_functions.md)

## Examples

``` r
test <- list(c(1.658, 0.5761), c(1e1, 785.6))

z <- format_xx("xx (xx.x)")
sapply(test, z)
#> [1] "2 (0.6)"    "10 (785.6)"

z <- format_xx("xx.x - xx.x")
sapply(test, z)
#> [1] "1.7 - 0.6"  "10 - 785.6"

z <- format_xx("xx.x, incl. xx.x% NE")
sapply(test, z)
#> [1] "1.7, incl. 0.6% NE"  "10, incl. 785.6% NE"
```
