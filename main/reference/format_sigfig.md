# Format numeric values by significant figures

Format numeric values to print with a specified number of significant
figures.

## Usage

``` r
format_sigfig(sigfig, format = "xx", num_fmt = "fg")
```

## Arguments

- sigfig:

  (`integer(1)`)  
  number of significant figures to display.

- format:

  (`string`)  
  the format label (string) to apply when printing the value. Decimal
  places in string are ignored in favor of formatting by significant
  figures. Formats options are: `"xx"`, `"xx / xx"`, `"(xx, xx)"`,
  `"xx - xx"`, and `"xx (xx)"`.

- num_fmt:

  (`string`)  
  numeric format modifiers to apply to the value. Defaults to `"fg"` for
  standard significant figures formatting - fixed (non-scientific
  notation) format (`"f"`) and `sigfig` equal to number of significant
  figures instead of decimal places (`"g"`). See the
  [`formatC()`](https://rdrr.io/r/base/formatc.html) `format` argument
  for more options.

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
[`format_xx()`](https://insightsengineering.github.io/tern/reference/format_xx.md),
[`formatting_functions`](https://insightsengineering.github.io/tern/reference/formatting_functions.md)

## Examples

``` r
fmt_3sf <- format_sigfig(3)
fmt_3sf(1.658)
#> [1] "1.66"
fmt_3sf(1e1)
#> [1] "10.0"

fmt_5sf <- format_sigfig(5)
fmt_5sf(0.57)
#> [1] "0.57000"
fmt_5sf(0.000025645)
#> [1] "0.000025645"
```
