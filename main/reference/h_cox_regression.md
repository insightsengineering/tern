# Helper functions for Cox proportional hazards regression

**\[stable\]**

Helper functions used in
[`fit_coxreg_univar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md)
and
[`fit_coxreg_multivar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md).

## Usage

``` r
h_coxreg_univar_formulas(variables, interaction = FALSE)

h_coxreg_multivar_formula(variables)

h_coxreg_univar_extract(effect, covar, data, mod, control = control_coxreg())

h_coxreg_multivar_extract(var, data, mod, control = control_coxreg())
```

## Arguments

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- interaction:

  (`flag`)  
  if `TRUE`, the model includes the interaction between the studied
  treatment and candidate covariate. Note that for univariate models
  without treatment arm, and multivariate models, no interaction can be
  used so that this needs to be `FALSE`.

- effect:

  (`string`)  
  the treatment variable.

- covar:

  (`string`)  
  the name of the covariate in the model.

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- mod:

  (`coxph`)  
  Cox regression model fitted by
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).

- control:

  (`list`)  
  a list of controls as returned by
  [`control_coxreg()`](https://insightsengineering.github.io/tern/reference/control_coxreg.md).

- var:

  (`string`)  
  single variable name that is passed by `rtables` when requested by a
  statistics function.

## Value

- `h_coxreg_univar_formulas()` returns a `character` vector coercible
  into formulas (e.g
  [`stats::as.formula()`](https://rdrr.io/r/stats/formula.html)).

&nbsp;

- `h_coxreg_multivar_formula()` returns a `string` coercible into a
  formula (e.g
  [`stats::as.formula()`](https://rdrr.io/r/stats/formula.html)).

&nbsp;

- `h_coxreg_univar_extract()` returns a `data.frame` with variables
  `effect`, `term`, `term_label`, `level`, `n`, `hr`, `lcl`, `ucl`, and
  `pval`.

&nbsp;

- `h_coxreg_multivar_extract()` returns a `data.frame` with variables
  `pval`, `hr`, `lcl`, `ucl`, `level`, `n`, `term`, and `term_label`.

## Functions

- `h_coxreg_univar_formulas()`: Helper for Cox regression formula.
  Creates a list of formulas. It is used internally by
  [`fit_coxreg_univar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md)
  for the comparison of univariate Cox regression models.

- `h_coxreg_multivar_formula()`: Helper for multivariate Cox regression
  formula. Creates a formulas string. It is used internally by
  [`fit_coxreg_multivar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md)
  for the comparison of multivariate Cox regression models. Interactions
  will not be included in multivariate Cox regression model.

- `h_coxreg_univar_extract()`: Utility function to help tabulate the
  result of a univariate Cox regression model.

- `h_coxreg_multivar_extract()`: Tabulation of multivariate Cox
  regressions. Utility function to help tabulate the result of a
  multivariate Cox regression model for a treatment/covariate variable.

## See also

[cox_regression](https://insightsengineering.github.io/tern/reference/cox_regression.md)

## Examples

``` r
# `h_coxreg_univar_formulas`

## Simple formulas.
h_coxreg_univar_formulas(
  variables = list(
    time = "time", event = "status", arm = "armcd", covariates = c("X", "y")
  )
)
#>                                        ref 
#>     "survival::Surv(time, status) ~ armcd" 
#>                                          X 
#> "survival::Surv(time, status) ~ armcd + X" 
#>                                          y 
#> "survival::Surv(time, status) ~ armcd + y" 

## Addition of an optional strata.
h_coxreg_univar_formulas(
  variables = list(
    time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
    strata = "SITE"
  )
)
#>                                                       ref 
#>     "survival::Surv(time, status) ~ armcd + strata(SITE)" 
#>                                                         X 
#> "survival::Surv(time, status) ~ armcd + X + strata(SITE)" 
#>                                                         y 
#> "survival::Surv(time, status) ~ armcd + y + strata(SITE)" 

## Inclusion of the interaction term.
h_coxreg_univar_formulas(
  variables = list(
    time = "time", event = "status", arm = "armcd", covariates = c("X", "y"),
    strata = "SITE"
  ),
  interaction = TRUE
)
#>                                                       ref 
#>     "survival::Surv(time, status) ~ armcd + strata(SITE)" 
#>                                                         X 
#> "survival::Surv(time, status) ~ armcd * X + strata(SITE)" 
#>                                                         y 
#> "survival::Surv(time, status) ~ armcd * y + strata(SITE)" 

## Only covariates fitted in separate models.
h_coxreg_univar_formulas(
  variables = list(
    time = "time", event = "status", covariates = c("X", "y")
  )
)
#>                                      X                                      y 
#> "survival::Surv(time, status) ~ 1 + X" "survival::Surv(time, status) ~ 1 + y" 

# `h_coxreg_multivar_formula`

h_coxreg_multivar_formula(
  variables = list(
    time = "AVAL", event = "event", arm = "ARMCD", covariates = c("RACE", "AGE")
  )
)
#> [1] "survival::Surv(AVAL, event) ~ ARMCD + RACE + AGE"

# Addition of an optional strata.
h_coxreg_multivar_formula(
  variables = list(
    time = "AVAL", event = "event", arm = "ARMCD", covariates = c("RACE", "AGE"),
    strata = "SITE"
  )
)
#> [1] "survival::Surv(AVAL, event) ~ ARMCD + RACE + AGE + strata(SITE)"

# Example without treatment arm.
h_coxreg_multivar_formula(
  variables = list(
    time = "AVAL", event = "event", covariates = c("RACE", "AGE"),
    strata = "SITE"
  )
)
#> [1] "survival::Surv(AVAL, event) ~ 1 + RACE + AGE + strata(SITE)"

library(survival)

dta_simple <- data.frame(
  time = c(5, 5, 10, 10, 5, 5, 10, 10),
  status = c(0, 0, 1, 0, 0, 1, 1, 1),
  armcd = factor(LETTERS[c(1, 1, 1, 1, 2, 2, 2, 2)], levels = c("A", "B")),
  var1 = c(45, 55, 65, 75, 55, 65, 85, 75),
  var2 = c("F", "M", "F", "M", "F", "M", "F", "U")
)
mod <- coxph(Surv(time, status) ~ armcd + var1, data = dta_simple)
result <- h_coxreg_univar_extract(
  effect = "armcd", covar = "armcd", mod = mod, data = dta_simple
)
result
#>       effect  term       term_label level n       hr       lcl      ucl
#> 1 Treatment: armcd B vs control (A)     B 8 6.551448 0.4606904 93.16769
#>       pval
#> 1 0.165209

mod <- coxph(Surv(time, status) ~ armcd + var1, data = dta_simple)
result <- h_coxreg_multivar_extract(
  var = "var1", mod = mod, data = dta_simple
)
result
#>        pval        hr      lcl      ucl level n term term_label
#> 2 0.4456195 0.9423284 0.808931 1.097724  var1 8 var1       var1
```
