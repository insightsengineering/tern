# Helper functions for subgroup treatment effect pattern (STEP) calculations

**\[stable\]**

Helper functions that are used internally for the STEP calculations.

## Usage

``` r
h_step_window(x, control = control_step())

h_step_trt_effect(data, model, variables, x)

h_step_survival_formula(variables, control = control_step())

h_step_survival_est(
  formula,
  data,
  variables,
  x,
  subset = rep(TRUE, nrow(data)),
  control = control_coxph()
)

h_step_rsp_formula(variables, control = c(control_step(), control_logistic()))

h_step_rsp_est(
  formula,
  data,
  variables,
  x,
  subset = rep(TRUE, nrow(data)),
  control = control_logistic()
)
```

## Arguments

- x:

  (`numeric`)  
  biomarker value(s) to use (without `NA`).

- control:

  (named `list`)  
  output from
  [`control_step()`](https://insightsengineering.github.io/tern/reference/control_step.md).

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- model:

  (`coxph` or `glm`)  
  the regression model object.

- variables:

  (named `list` of `string`)  
  list of additional analysis variables.

- formula:

  (`formula`)  
  the regression model formula.

- subset:

  (`logical`)  
  subset vector.

## Value

- `h_step_window()` returns a list containing the window-selection
  matrix `sel` and the interval information matrix `interval`.

&nbsp;

- `h_step_trt_effect()` returns a vector with elements `est` and `se`.

&nbsp;

- `h_step_survival_formula()` returns a model formula.

&nbsp;

- `h_step_survival_est()` returns a matrix of number of observations
  `n`, `events`, log hazard ratio estimates `loghr`, standard error
  `se`, and Wald confidence interval bounds `ci_lower` and `ci_upper`.
  One row is included for each biomarker value in `x`.

&nbsp;

- `h_step_rsp_formula()` returns a model formula.

&nbsp;

- `h_step_rsp_est()` returns a matrix of number of observations `n`, log
  odds ratio estimates `logor`, standard error `se`, and Wald confidence
  interval bounds `ci_lower` and `ci_upper`. One row is included for
  each biomarker value in `x`.

## Functions

- `h_step_window()`: Creates the windows for STEP, based on the control
  settings provided.

- `h_step_trt_effect()`: Calculates the estimated treatment effect
  estimate on the linear predictor scale and corresponding standard
  error from a STEP `model` fitted on `data` given `variables`
  specification, for a single biomarker value `x`. This works for both
  `coxph` and `glm` models, i.e. for calculating log hazard ratio or log
  odds ratio estimates.

- `h_step_survival_formula()`: Builds the model formula used in survival
  STEP calculations.

- `h_step_survival_est()`: Estimates the model with `formula` built
  based on `variables` in `data` for a given `subset` and `control`
  parameters for the Cox regression.

- `h_step_rsp_formula()`: Builds the model formula used in response STEP
  calculations.

- `h_step_rsp_est()`: Estimates the model with `formula` built based on
  `variables` in `data` for a given `subset` and `control` parameters
  for the logistic regression.
