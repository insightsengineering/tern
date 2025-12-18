# Format automatically using data significant digits

**\[stable\]**

Formatting function for the majority of default methods used in
[`analyze_vars()`](https://insightsengineering.github.io/tern/reference/analyze_variables.md).
For non-derived values, the significant digits of data is used (e.g.
range), while derived values have one more digits (measure of location
and dispersion like mean, standard deviation). This function can be
called internally with "auto" like, for example,
`.formats = c("mean" = "auto")`. See details to see how this works with
the inner function.

## Usage

``` r
format_auto(dt_var, x_stat)
```

## Arguments

- dt_var:

  (`numeric`)  
  variable data the statistics were calculated from. Used only to find
  significant digits. In
  [analyze_vars](https://insightsengineering.github.io/tern/reference/analyze_variables.md)
  this comes from `.df_row` (see
  [rtables::additional_fun_params](https://insightsengineering.github.io/rtables/latest-tag/reference/additional_fun_params.html)),
  and it is the row data after the above row splits. No column split is
  considered.

- x_stat:

  (`string`)  
  string indicating the current statistical method used.

## Value

A string that `rtables` prints in a table cell.

## Details

The internal function is needed to work with `rtables` default structure
for format functions, i.e. `function(x, ...)`, where is x are results
from statistical evaluation. It can be more than one element (e.g. for
`.stats = "mean_sd"`).

## See also

Other formatting functions:
[`extreme_format`](https://insightsengineering.github.io/tern/reference/extreme_format.md),
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
x_todo <- c(0.001, 0.2, 0.0011000, 3, 4)
res <- c(mean(x_todo[1:3]), sd(x_todo[1:3]))

# x is the result coming into the formatting function -> res!!
format_auto(dt_var = x_todo, x_stat = "mean_sd")(x = res)
#> [1] "0.06737 (0.11486)"
format_auto(x_todo, "range")(x = range(x_todo))
#> [1] "0.0010 - 4.0000"
no_sc_x <- c(0.0000001, 1)
format_auto(no_sc_x, "range")(x = no_sc_x)
#> [1] "0.0000001 - 1.0000000"
```
