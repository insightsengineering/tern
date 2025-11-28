# Control function for `survfit` models for survival time

**\[stable\]**

This is an auxiliary function for controlling arguments for `survfit`
model, typically used internally to specify details of `survfit` model
for
[`s_surv_time()`](https://insightsengineering.github.io/tern/reference/survival_time.md).
`conf_level` refers to survival time estimation.

## Usage

``` r
control_surv_time(
  conf_level = 0.95,
  conf_type = c("plain", "log", "log-log"),
  quantiles = c(0.25, 0.75)
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

- quantiles:

  (`numeric(2)`)  
  vector of length two specifying the quantiles of survival time.

## Value

A list of components with the same names as the arguments.
