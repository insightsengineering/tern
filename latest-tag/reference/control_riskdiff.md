# Control function for risk difference column

**\[stable\]**

Sets a list of parameters to use when generating a risk (proportion)
difference column. Used as input to the `riskdiff` parameter of
[`tabulate_rsp_subgroups()`](https://insightsengineering.github.io/tern/reference/response_subgroups.md)
and
[`tabulate_survival_subgroups()`](https://insightsengineering.github.io/tern/reference/survival_duration_subgroups.md).

## Usage

``` r
control_riskdiff(
  arm_x = NULL,
  arm_y = NULL,
  format = "xx.x (xx.x - xx.x)",
  col_label = "Risk Difference (%) (95% CI)",
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

- format:

  (`string` or `function`)  
  the format label (string) or formatting function to apply to the risk
  difference statistic. See the `3d` string options in
  [`formatters::list_valid_format_labels()`](https://insightsengineering.github.io/formatters/latest-tag/reference/list_formats.html)
  for possible format strings. Defaults to `"xx.x (xx.x - xx.x)"`.

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

A `list` of items with names corresponding to the arguments.

## See also

[`add_riskdiff()`](https://insightsengineering.github.io/tern/reference/add_riskdiff.md),
[`tabulate_rsp_subgroups()`](https://insightsengineering.github.io/tern/reference/response_subgroups.md),
and
[`tabulate_survival_subgroups()`](https://insightsengineering.github.io/tern/reference/survival_duration_subgroups.md).

## Examples

``` r
control_riskdiff()
#> $arm_x
#> NULL
#> 
#> $arm_y
#> NULL
#> 
#> $format
#> [1] "xx.x (xx.x - xx.x)"
#> 
#> $col_label
#> [1] "Risk Difference (%) (95% CI)"
#> 
#> $pct
#> [1] TRUE
#> 
control_riskdiff(arm_x = "ARM A", arm_y = "ARM B")
#> $arm_x
#> [1] "ARM A"
#> 
#> $arm_y
#> [1] "ARM B"
#> 
#> $format
#> [1] "xx.x (xx.x - xx.x)"
#> 
#> $col_label
#> [1] "Risk Difference (%) (95% CI)"
#> 
#> $pct
#> [1] TRUE
#> 
```
