# Subgroup treatment effect pattern (STEP) fit for binary (response) outcome

**\[stable\]**

This fits the Subgroup Treatment Effect Pattern logistic regression
models for a binary (response) outcome. The treatment arm variable must
have exactly 2 levels, where the first one is taken as reference and the
estimated odds ratios are for the comparison of the second level vs. the
first one.

The (conditional) logistic regression model which is fit is:

`response ~ arm * poly(biomarker, degree) + covariates + strata(strata)`

where `degree` is specified by
[`control_step()`](https://insightsengineering.github.io/tern/reference/control_step.md).

## Usage

``` r
fit_rsp_step(variables, data, control = c(control_step(), control_logistic()))
```

## Arguments

- variables:

  (named `list` of `character`)  
  list of analysis variables: needs `response`, `arm`, `biomarker`, and
  optional `covariates` and `strata`.

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- control:

  (named `list`)  
  combined control list from
  [`control_step()`](https://insightsengineering.github.io/tern/reference/control_step.md)
  and
  [`control_logistic()`](https://insightsengineering.github.io/tern/reference/control_logistic.md).

## Value

A matrix of class `step`. The first part of the columns describe the
subgroup intervals used for the biomarker variable, including where the
center of the intervals are and their bounds. The second part of the
columns contain the estimates for the treatment arm comparison.

## Note

For the default degree 0 the `biomarker` variable is not included in the
model.

## See also

[`control_step()`](https://insightsengineering.github.io/tern/reference/control_step.md)
and
[`control_logistic()`](https://insightsengineering.github.io/tern/reference/control_logistic.md)
for the available customization options.

## Examples

``` r
# Testing dataset with just two treatment arms.
library(survival)
library(dplyr)

adrs_f <- tern_ex_adrs %>%
  filter(
    PARAMCD == "BESRSPI",
    ARM %in% c("B: Placebo", "A: Drug X")
  ) %>%
  mutate(
    # Reorder levels of ARM to have Placebo as reference arm for Odds Ratio calculations.
    ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
    RSP = case_when(AVALC %in% c("PR", "CR") ~ 1, TRUE ~ 0),
    SEX = factor(SEX)
  )

variables <- list(
  arm = "ARM",
  biomarker = "BMRKR1",
  covariates = "AGE",
  response = "RSP"
)

# Fit default STEP models: Here a constant treatment effect is estimated in each subgroup.
# We use a large enough bandwidth to avoid too small subgroups and linear separation in those.
step_matrix <- fit_rsp_step(
  variables = variables,
  data = adrs_f,
  control = c(control_logistic(), control_step(bandwidth = 0.9))
)
dim(step_matrix)
#> [1] 39 11
head(step_matrix)
#>      Percentile Center Percentile Lower Percentile Upper Interval Center
#> [1,]             0.025                0            0.925        1.472755
#> [2,]             0.050                0            0.950        2.028546
#> [3,]             0.075                0            0.975        2.204086
#> [4,]             0.100                0            1.000        2.799776
#> [5,]             0.125                0            1.000        2.969998
#> [6,]             0.150                0            1.000        3.149068
#>      Interval Lower Interval Upper   n    logor       se   ci_lower ci_upper
#> [1,]      0.4459546       11.74529 131 1.835008 1.134560 -0.3886885 4.058703
#> [2,]      0.4459546       12.73387 134 1.895158 1.136248 -0.3318481 4.122163
#> [3,]      0.4459546       14.75099 138 1.947699 1.136969 -0.2807193 4.176116
#> [4,]      0.4459546       18.49236 142 1.931001 1.135866 -0.2952561 4.157258
#> [5,]      0.4459546       18.49236 142 1.931001 1.135866 -0.2952561 4.157258
#> [6,]      0.4459546       18.49236 142 1.931001 1.135866 -0.2952561 4.157258

# Specify different polynomial degree for the biomarker interaction to use more flexible local
# models. Or specify different logistic regression options, including confidence level.
step_matrix2 <- fit_rsp_step(
  variables = variables,
  data = adrs_f,
  control = c(control_logistic(conf_level = 0.9), control_step(bandwidth = NULL, degree = 1))
)

# Use a global constant model. This is helpful as a reference for the subgroup models.
step_matrix3 <- fit_rsp_step(
  variables = variables,
  data = adrs_f,
  control = c(control_logistic(), control_step(bandwidth = NULL, num_points = 2L))
)

# It is also possible to use strata, i.e. use conditional logistic regression models.
variables2 <- list(
  arm = "ARM",
  biomarker = "BMRKR1",
  covariates = "AGE",
  response = "RSP",
  strata = c("STRATA1", "STRATA2")
)

step_matrix4 <- fit_rsp_step(
  variables = variables2,
  data = adrs_f,
  control = c(control_logistic(), control_step(bandwidth = NULL))
)
```
