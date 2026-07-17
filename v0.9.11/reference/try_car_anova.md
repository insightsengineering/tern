# `tryCatch` around `car::Anova`

Captures warnings when executing
[car::Anova](https://rdrr.io/pkg/car/man/Anova.html).

## Usage

``` r
try_car_anova(mod, test.statistic)
```

## Arguments

- mod:

  `lm`, `aov`, `glm`, `multinom`, `polr` `mlm`, `coxph`, `coxme`, `lme`,
  `mer`, `merMod`, `svyglm`, `svycoxph`, `rlm`, `clm`, `clmm`, or other
  suitable model object.

- test.statistic:

  for a generalized linear model, whether to calculate `"LR"`
  (likelihood-ratio), `"Wald"`, or `"F"` tests; for a Cox or Cox
  mixed-effects model, whether to calculate `"LR"` (partial-likelihood
  ratio) or `"Wald"` tests (with `"LR"` tests unavailable for Cox models
  using the `tt` argument); in the default case or for linear mixed
  models fit by `lmer`, whether to calculate Wald `"Chisq"` or
  Kenward-Roger `"F"` tests with Satterthwaite degrees of freedom
  (*warning:* the KR F-tests can be very time-consuming). For a
  multivariate linear model, the multivariate test statistic to compute
  — one of `"Pillai"`, `"Wilks"`, `"Hotelling-Lawley"`, or `"Roy"`, with
  `"Pillai"` as the default. The `summary` method for `Anova.mlm`
  objects permits the specification of more than one multivariate test
  statistic, and the default is to report all four.

## Value

A list with item `aov` for the result of the model and `error_text` for
the captured warnings.

## Examples

``` r
# `car::Anova` on cox regression model including strata and expected
# a likelihood ratio test triggers a warning as only Wald method is
# accepted.

library(survival)

mod <- coxph(
  formula = Surv(time = futime, event = fustat) ~ factor(rx) + strata(ecog.ps),
  data = ovarian
)
```
