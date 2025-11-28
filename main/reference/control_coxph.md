# Control function for Cox-PH model

**\[stable\]**

This is an auxiliary function for controlling arguments for Cox-PH
model, typically used internally to specify details of Cox-PH model for
[`s_coxph_pairwise()`](https://insightsengineering.github.io/tern/reference/survival_coxph_pairwise.md).
`conf_level` refers to Hazard Ratio estimation.

## Usage

``` r
control_coxph(
  pval_method = c("log-rank", "wald", "likelihood"),
  ties = c("efron", "breslow", "exact"),
  conf_level = 0.95
)
```

## Arguments

- pval_method:

  (`string`)  
  p-value method for testing hazard ratio = 1. Default method is
  `"log-rank"`, can also be set to `"wald"` or `"likelihood"`.

- ties:

  (`string`)  
  string specifying the method for tie handling. Default is `"efron"`,
  can also be set to `"breslow"` or `"exact"`. See more in
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

- conf_level:

  (`proportion`)  
  confidence level of the interval.

## Value

A list of components with the same names as the arguments.
