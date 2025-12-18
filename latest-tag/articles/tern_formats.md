# Formatting Functions

## `tern` Formatting Functions Overview

The `tern` R package provides functions to create common analyses from
clinical trials in `R` and these functions have default formatting
arguments for displaying the values in the output a specific way.

`tern` formatting differs compared to the formatting available in the
`formatters` package as `tern` formats are capable of handling logical
statements, allowing for more fine-tuning of the output displayed.
Depending on what type of value is being displayed, and what that value
is, the format of the output will change. Whereas when using the
`formatters` package, the specified format is applied regardless of the
value.

To see the available formatting functions available in `tern` see
[`?formatting_functions`](https://insightsengineering.github.io/tern/reference/formatting_functions.md).
To see the available format strings available in `formatters` see
[`formatters::list_valid_format_labels()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html).

## Comparing `tern` & `formatters` Formats

The packages used in this vignette are:

``` r
library(rtables)
library(formatters)
library(tern)
library(dplyr)
```

The example below demonstrates the use of `tern` formatting in the
[`count_abnormal()`](https://insightsengineering.github.io/tern/reference/abnormal.md)
function. The example “low” category has a non-zero numerator value so
both a fraction and a percentage value are displayed, while the “high”
value has a numerator value of zero and so the fraction value is
displayed without also displaying the redundant zero percentage value.

``` r
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "LOW")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)

df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = format_fraction)
  ) %>%
  build_table(df2)
#>         all obs  
#> —————————————————
#> low    2/2 (100%)
#> high      0/2
```

In the following example the
[`count_abnormal()`](https://insightsengineering.github.io/tern/reference/abnormal.md)
function is utilized again. This time both “low” values and “high”
values have a non-zero numerator and so both show a percentage.

``` r
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "HIGH")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)

df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = format_fraction)
  ) %>%
  build_table(df2)
#>         all obs 
#> ————————————————
#> low    1/2 (50%)
#> high   1/2 (50%)
```

The following example demonstrates the difference when `formatters` is
used instead to format the output. Here we choose to use `"xx / xx"` as
our value format. The “high” value has a zero numerator value and the
“low” value has a non-zero numerator, yet both are displayed in the same
format.

``` r
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "LOW")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)
df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = "xx / xx")
  ) %>%
  build_table(df2)
#>        all obs
#> ——————————————
#> low     2 / 2 
#> high    0 / 2
```

The same concept occurs when using any of the available formats from the
`formatters` package. The following example displays the same result
using the `"xx.x / xx.x"` format instead. Use
[`formatters::list_valid_format_labels()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html)
to see the full list of available formats in `formatters`.

``` r
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "LOW")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
)
df2 <- df2 %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = "xx.x / xx.x")
  ) %>%
  build_table(df2)
#>         all obs 
#> ————————————————
#> low    2.0 / 2.0
#> high   0.0 / 2.0
```

## Formatting Function Basics

Current `tern` formatting functions consider some of the following
aspects when setting custom behaviors:

- Missing values - a custom value or string can be set to display for
  missing values instead of `NA`.
- 0’s - if a cell value is zero, `tern` fraction formatting functions
  will exclude the accompanying percentage value.
- Number of decimal places to display - the number of decimal places can
  be fixed if needed.
- Value thresholds - a different format or value can be displayed
  depending on whether the value is within a certain threshold.

#### Number of Decimal Places to Display

Two functions that set a fixed number of decimal places (specifically 1)
are
[`format_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_fraction_fixed_dp.md)
and
[`format_count_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_count_fraction_fixed_dp.md).
By default, formatting functions will remove trailing zeros, but these
two functions will always have one decimal place in their percentage,
even if the digit is a zero. See the following example:

``` r
format_fraction_fixed_dp(x = c(num = 1L, denom = 3L))
#> [1] "1/3 (33.3%)"
format_fraction_fixed_dp(x = c(num = 1L, denom = 2L))
#> [1] "1/2 (50.0%)"

format_count_fraction_fixed_dp(x = c(2, 0.6667))
#> [1] "2 (66.7%)"
format_count_fraction_fixed_dp(x = c(2, 0.25))
#> [1] "2 (25.0%)"
```

#### Value Thresholds

Functions that set custom values according to a certain threshold
include
[`format_extreme_values()`](https://insightsengineering.github.io/tern/reference/format_extreme_values.md),
[`format_extreme_values_ci()`](https://insightsengineering.github.io/tern/reference/format_extreme_values_ci.md),
and
[`format_fraction_threshold()`](https://insightsengineering.github.io/tern/reference/format_fraction_threshold.md).
The extreme value formats work similarly to allow the user to specify
the maximum number of digits to include, and very large or very small
values are given a special string value. For example:

``` r
extreme_format <- format_extreme_values(digits = 2)
extreme_format(0.235)
#> [1] "0.23"
extreme_format(0.001)
#> [1] "<0.01"
extreme_format(Inf)
#> [1] ">999.99"
```

The
[`format_fraction_threshold()`](https://insightsengineering.github.io/tern/reference/format_fraction_threshold.md)
function allows the user to specify a lower percentage threshold, below
which values are instead assigned a special string value. For example:

``` r
fraction_format <- format_fraction_threshold(0.05)
fraction_format(x = c(20, 0.1))
#> [1] 10
fraction_format(x = c(2, 0.01))
#> [1] "<5"
```

See the documentation on each function for specific details on their
behavior and how to customize them.

## Creating Custom Formatting Functions

If your table requires customized output that cannot be displayed using
one of the pre-existing `tern` formatting functions, you may want to
consider creating a new formatting function. When creating your own
formatting function it is important to consider the aspects listed in
the Formatting Function Customization section above.

In this section we will create a custom formatting function derived from
the
[`format_fraction_fixed_dp()`](https://insightsengineering.github.io/tern/reference/format_fraction_fixed_dp.md)
function. First we will take a look at this function in detail and then
we will customize it.

``` r
# First we will see how the format_fraction_fixed_dp code works and displays the outputs
format_fraction_fixed_dp <- function(x, ...) {
  attr(x, "label") <- NULL
  checkmate::assert_vector(x)
  checkmate::assert_count(x["num"])
  checkmate::assert_count(x["denom"])

  result <- if (x["num"] == 0) {
    paste0(x["num"], "/", x["denom"])
  } else {
    paste0(
      x["num"], "/", x["denom"],
      " (", sprintf("%.1f", round(x["num"] / x["denom"] * 100, 1)), "%)"
    )
  }
  return(result)
}
```

Here we see that if the numerator value is greater than 0, the fraction
and percentage is displayed. If the numerator is 0, only the fraction is
shown. Percent values always display 1 decimal place. Below we will
create a dummy dataset and then observe the output value behavior when
this formatting function is applied.

``` r
df2 <- data.frame(
  ID = as.character(c(1, 1, 2, 2)),
  RANGE = factor(c("NORMAL", "LOW", "HIGH", "LOW")),
  BL_RANGE = factor(c("NORMAL", "NORMAL", "HIGH", "HIGH")),
  ONTRTFL = c("", "Y", "", "Y"),
  stringsAsFactors = FALSE
) %>%
  filter(ONTRTFL == "Y")

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = format_fraction_fixed_dp)
  ) %>%
  build_table(df2)
#>          all obs   
#> ———————————————————
#> low    2/2 (100.0%)
#> high       0/2
```

Now we will modify this function to make our custom formatting function,
`custom_format`. We want to display 3 decimal places in the percent
value, and if the numerator value is 0 we only want to display a 0 value
(without the denominator).

``` r
custom_format <- function(x, ...) {
  attr(x, "label") <- NULL
  checkmate::assert_vector(x)
  checkmate::assert_count(x["num"])
  checkmate::assert_count(x["denom"])

  result <- if (x["num"] == 0) {
    paste0(x["num"]) # We remove the denominator on this line so that only a 0 is displayed
  } else {
    paste0(
      x["num"], "/", x["denom"],
      " (", sprintf("%.3f", round(x["num"] / x["denom"] * 100, 1)), "%)" # We include 3 decimal places with %.3f
    )
  }
  return(result)
}

basic_table() %>%
  count_abnormal(
    var = "RANGE",
    abnormal = list(low = "LOW", high = "HIGH"),
    variables = list(id = "ID", baseline = "BL_RANGE"),
    exclude_base_abn = FALSE,
    .formats = list(fraction = custom_format) # Here we implement our new custom_format function
  ) %>%
  build_table(df2)
#>           all obs    
#> —————————————————————
#> low    2/2 (100.000%)
#> high         0
```

## Summary

Each `tern` analysis function has pre-specified default format functions
to implement when generating output, some of which are taken from the
`formatters` package and some of which are custom formatting functions
stored in `tern`. These `tern` functions differ compared to those from
`formatters` in that logical statements can be used to set
value-dependent customized formats. If you would like to create your own
custom formatting function to use with `tern`, be sure to carefully
consider which rules you want to implement to handle different input
values.
