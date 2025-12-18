# Split function to configure risk difference column

**\[stable\]**

Wrapper function for
[`rtables::add_combo_levels()`](https://insightsengineering.github.io/rtables/latest-tag/reference/add_overall_level.html)
which configures settings for the risk difference column to be added to
an `rtables` object. To add a risk difference column to a table, this
function should be used as `split_fun` in calls to
[`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html),
followed by setting argument `riskdiff` to `TRUE` in all following
analyze function calls.

## Usage

``` r
add_riskdiff(
  arm_x,
  arm_y,
  col_label = paste0("Risk Difference (%) (95% CI)", if (length(arm_y) > 1)
    paste0("\n", arm_x, " vs. ", arm_y)),
  pct = TRUE
)
```

## Arguments

- arm_x:

  (`string`)  
  name of reference arm to use in risk difference calculations.

- arm_y:

  (`character`)  
  names of one or more arms to compare to reference arm in risk
  difference calculations. A new column will be added for each value of
  `arm_y`.

- col_label:

  (`character`)  
  labels to use when rendering the risk difference column within the
  table. If more than one comparison arm is specified in `arm_y`,
  default labels will specify which two arms are being compared
  (reference arm vs. comparison arm).

- pct:

  (`flag`)  
  whether output should be returned as percentages. Defaults to `TRUE`.

## Value

A closure suitable for use as a split function (`split_fun`) within
[`rtables::split_cols_by()`](https://insightsengineering.github.io/rtables/latest-tag/reference/split_cols_by.html)
when creating a table layout.

## See also

[`stat_propdiff_ci()`](https://insightsengineering.github.io/tern/reference/stat_propdiff_ci.md)
for details on risk difference calculation.

## Examples

``` r
adae <- tern_ex_adae
adae$AESEV <- factor(adae$AESEV)

lyt <- basic_table() %>%
  split_cols_by("ARMCD", split_fun = add_riskdiff(arm_x = "ARM A", arm_y = c("ARM B", "ARM C"))) %>%
  count_occurrences_by_grade(
    var = "AESEV",
    riskdiff = TRUE
  )

tbl <- build_table(lyt, df = adae)
tbl
#>                                                   Risk Difference (%) (95% CI)   Risk Difference (%) (95% CI)
#>              ARM A        ARM B        ARM C            ARM A vs. ARM B                ARM A vs. ARM C       
#> —————————————————————————————————————————————————————————————————————————————————————————————————————————————
#> MILD        6 (3.0%)     4 (2.3%)     2 (1.2%)          0.7 (-2.5 - 3.9)               1.7 (-1.2 - 4.6)      
#> MODERATE   19 (9.4%)    15 (8.5%)    14 (8.6%)          0.9 (-4.8 - 6.7)               0.8 (-5.1 - 6.7)      
#> SEVERE     34 (16.8%)   38 (21.5%)   32 (19.8%)        -4.6 (-12.6 - 3.3)             -2.9 (-10.9 - 5.1)     
```
