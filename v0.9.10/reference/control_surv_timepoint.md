# Control function for `survfit` models for patients' survival rate at time points

**\[stable\]**

This is an auxiliary function for controlling arguments for `survfit`
model, typically used internally to specify details of `survfit` model
for
[`s_surv_timepoint()`](https://insightsengineering.github.io/tern/reference/survival_timepoint.md).
`conf_level` refers to patient risk estimation at a time point.

## Usage

``` r
control_surv_timepoint(
  conf_level = 0.95,
  conf_type = c("plain", "log", "log-log")
)
```

## Arguments

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- conf_type:

  (`string`)  
  confidence interval type. Options are "plain" (default), "log",
  "log-log", see more in
  [`survival::survfit()`](https://rdrr.io/pkg/survival/man/survfit.html).
  Note option "none" is no longer supported.

## Value

A list of components with the same names as the arguments.
