# Format extreme values

**\[stable\]**

`rtables` formatting functions that handle extreme values.

## Usage

``` r
h_get_format_threshold(digits = 2L)

h_format_threshold(x, digits = 2L)
```

## Arguments

- digits:

  (`integer(1)`)  
  number of decimal places to display.

- x:

  (`numeric(1)`)  
  value to format.

## Value

- `h_get_format_threshold()` returns a `list` of 2 elements:
  `threshold`, with `low` and `high` thresholds, and `format_string`,
  with thresholds formatted as strings.

&nbsp;

- `h_format_threshold()` returns the given value, or if the value is not
  within the digit threshold the relation of the given value to the
  digit threshold, as a formatted string.

## Details

For each input, apply a format to the specified number of `digits`. If
the value is below a threshold, it returns "\<0.01" e.g. if the number
of `digits` is 2. If the value is above a threshold, it returns
"\>999.99" e.g. if the number of `digits` is 2. If it is zero, then
returns "0.00".

## Functions

- `h_get_format_threshold()`: Internal helper function to calculate the
  threshold and create formatted strings used in Formatting Functions.
  Returns a list with elements `threshold` and `format_string`.

- `h_format_threshold()`: Internal helper function to apply a threshold
  format to a value. Creates a formatted string to be used in Formatting
  Functions.

## See also

Other formatting functions:
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
h_get_format_threshold(2L)
#> $threshold
#>    low   high 
#>   0.01 999.99 
#> 
#> $format_string
#>       low      high 
#>   "<0.01" ">999.99" 
#> 

h_format_threshold(0.001)
#> [1] "<0.01"
h_format_threshold(1000)
#> [1] ">999.99"
```
