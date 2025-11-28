# Control function for incidence rate

**\[stable\]**

This is an auxiliary function for controlling arguments for the
incidence rate, used internally to specify details in
[`s_incidence_rate()`](https://insightsengineering.github.io/tern/reference/incidence_rate.md).

## Usage

``` r
control_incidence_rate(
  conf_level = 0.95,
  conf_type = c("normal", "normal_log", "exact", "byar"),
  input_time_unit = c("year", "day", "week", "month"),
  num_pt_year = 100
)
```

## Arguments

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- conf_type:

  (`string`)  
  `normal` (default), `normal_log`, `exact`, or `byar` for confidence
  interval type.

- input_time_unit:

  (`string`)  
  `day`, `week`, `month`, or `year` (default) indicating time unit for
  data input.

- num_pt_year:

  (`numeric(1)`)  
  number of patient-years to use when calculating adverse event rates.

## Value

A list of components with the same names as the arguments.

## See also

[incidence_rate](https://insightsengineering.github.io/tern/reference/incidence_rate.md)

## Examples

``` r
control_incidence_rate(0.9, "exact", "month", 100)
#> $conf_level
#> [1] 0.9
#> 
#> $conf_type
#> [1] "exact"
#> 
#> $input_time_unit
#> [1] "month"
#> 
#> $num_pt_year
#> [1] 100
#> 
```
