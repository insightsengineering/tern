# Hazard ratio estimation in interactions

This function estimates the hazard ratios between arms when an
interaction variable is given with specific values.

## Usage

``` r
estimate_coef(
  variable,
  given,
  lvl_var,
  lvl_given,
  coef,
  mmat,
  vcov,
  conf_level = 0.95
)
```

## Arguments

- variable, given:

  (`character(2)`)  
  names of the two variables in the interaction. We seek the estimation
  of the levels of `variable` given the levels of `given`.

- lvl_var, lvl_given:

  (`character`)  
  corresponding levels given by
  [`levels()`](https://rdrr.io/r/base/levels.html).

- coef:

  (`numeric`)  
  vector of estimated coefficients.

- mmat:

  (named `numeric`) a vector filled with `0`s used as a template to
  obtain the design matrix.

- vcov:

  (`matrix`)  
  variance-covariance matrix of underlying model.

- conf_level:

  (`proportion`)  
  confidence level of estimate intervals.

## Value

A list of matrices (one per level of variable) with rows corresponding
to the combinations of `variable` and `given`, with columns:

- `coef_hat`: Estimation of the coefficient.

- `coef_se`: Standard error of the estimation.

- `hr`: Hazard ratio.

- `lcl, ucl`: Lower/upper confidence limit of the hazard ratio.

## Details

Given the cox regression investigating the effect of Arm (A, B, C;
reference A) and Sex (F, M; reference Female). The model is abbreviated:
y ~ Arm + Sex + Arm x Sex. The cox regression estimates the coefficients
along with a variance-covariance matrix for:

- b1 (arm b), b2 (arm c)

- b3 (sex m)

- b4 (arm b: sex m), b5 (arm c: sex m)

Given that I want an estimation of the Hazard Ratio for arm C/sex M, the
estimation will be given in reference to arm A/Sex M by exp(b2 + b3 +
b5)/ exp(b3) = exp(b2 + b5), therefore the interaction coefficient is
given by b2 + b5 while the standard error is obtained as \$1.96 \*
sqrt(Var b2 + Var b5 + 2 \* covariance (b2,b5))\$ for a confidence level
of 0.95.

## See also

[`s_cox_multivariate()`](https://insightsengineering.github.io/tern/reference/s_cox_multivariate.md).

## Examples

``` r
library(dplyr)
library(survival)

ADSL <- tern_ex_adsl %>%
  filter(SEX %in% c("F", "M"))

adtte <- tern_ex_adtte %>% filter(PARAMCD == "PFS")
adtte$ARMCD <- droplevels(adtte$ARMCD)
adtte$SEX <- droplevels(adtte$SEX)

mod <- coxph(
  formula = Surv(time = AVAL, event = 1 - CNSR) ~ (SEX + ARMCD)^2,
  data = adtte
)

mmat <- stats::model.matrix(mod)[1, ]
mmat[!mmat == 0] <- 0
```
