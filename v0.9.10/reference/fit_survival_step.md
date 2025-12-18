# Subgroup treatment effect pattern (STEP) fit for survival outcome

**\[stable\]**

This fits the subgroup treatment effect pattern (STEP) models for a
survival outcome. The treatment arm variable must have exactly 2 levels,
where the first one is taken as reference and the estimated hazard
ratios are for the comparison of the second level vs. the first one.

The model which is fit is:

`Surv(time, event) ~ arm * poly(biomarker, degree) + covariates + strata(strata)`

where `degree` is specified by
[`control_step()`](https://insightsengineering.github.io/tern/reference/control_step.md).

## Usage

``` r
fit_survival_step(
  variables,
  data,
  control = c(control_step(), control_coxph())
)
```

## Arguments

- variables:

  (named `list` of `character`)  
  list of analysis variables: needs `time`, `event`, `arm`, `biomarker`,
  and optional `covariates` and `strata`.

- data:

  (`data.frame`)  
  the dataset containing the variables to summarize.

- control:

  (named `list`)  
  combined control list from
  [`control_step()`](https://insightsengineering.github.io/tern/reference/control_step.md)
  and
  [`control_coxph()`](https://insightsengineering.github.io/tern/reference/control_coxph.md).

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
[`control_coxph()`](https://insightsengineering.github.io/tern/reference/control_coxph.md)
for the available customization options.

## Examples

``` r
# Testing dataset with just two treatment arms.
library(dplyr)

adtte_f <- tern_ex_adtte %>%
  filter(
    PARAMCD == "OS",
    ARM %in% c("B: Placebo", "A: Drug X")
  ) %>%
  mutate(
    # Reorder levels of ARM to display reference arm before treatment arm.
    ARM = droplevels(forcats::fct_relevel(ARM, "B: Placebo")),
    is_event = CNSR == 0
  )
labels <- c("ARM" = "Treatment Arm", "is_event" = "Event Flag")
formatters::var_labels(adtte_f)[names(labels)] <- labels

variables <- list(
  arm = "ARM",
  biomarker = "BMRKR1",
  covariates = c("AGE", "BMRKR2"),
  event = "is_event",
  time = "AVAL"
)

# Fit default STEP models: Here a constant treatment effect is estimated in each subgroup.
step_matrix <- fit_survival_step(
  variables = variables,
  data = adtte_f
)
dim(step_matrix)
#> [1] 39 12
head(step_matrix)
#>      Percentile Center Percentile Lower Percentile Upper Interval Center
#> [1,]             0.025                0            0.275        1.472755
#> [2,]             0.050                0            0.300        2.028546
#> [3,]             0.075                0            0.325        2.204086
#> [4,]             0.100                0            0.350        2.799776
#> [5,]             0.125                0            0.375        2.969998
#> [6,]             0.150                0            0.400        3.149068
#>      Interval Lower Interval Upper  n events      loghr        se   ci_lower
#> [1,]      0.4459546       4.182444 39     29 0.03729601 0.4243013 -0.7943192
#> [2,]      0.4459546       4.349471 43     31 0.23246622 0.3881347 -0.5282639
#> [3,]      0.4459546       4.626913 46     32 0.07952492 0.3813113 -0.6678314
#> [4,]      0.4459546       4.805767 50     35 0.09020455 0.3638368 -0.6229026
#> [5,]      0.4459546       4.929816 53     37 0.15522702 0.3477999 -0.5264483
#> [6,]      0.4459546       5.020539 57     41 0.11135760 0.3332843 -0.5418676
#>       ci_upper
#> [1,] 0.8689112
#> [2,] 0.9931963
#> [3,] 0.8268813
#> [4,] 0.8033117
#> [5,] 0.8369023
#> [6,] 0.7645828

# Specify different polynomial degree for the biomarker interaction to use more flexible local
# models. Or specify different Cox regression options.
step_matrix2 <- fit_survival_step(
  variables = variables,
  data = adtte_f,
  control = c(control_coxph(conf_level = 0.9), control_step(degree = 2))
)

# Use a global model with cubic interaction and only 5 points.
step_matrix3 <- fit_survival_step(
  variables = variables,
  data = adtte_f,
  control = c(control_coxph(), control_step(bandwidth = NULL, degree = 3, num_points = 5L))
)
```
