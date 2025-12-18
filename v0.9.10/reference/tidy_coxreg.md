# Custom tidy methods for Cox regression

**\[stable\]**

## Usage

``` r
# S3 method for class 'summary.coxph'
tidy(x, ...)

# S3 method for class 'coxreg.univar'
tidy(x, ...)

# S3 method for class 'coxreg.multivar'
tidy(x, ...)
```

## Arguments

- x:

  (`list`)  
  result of the Cox regression model fitted by
  [`fit_coxreg_univar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md)
  (for univariate models) or
  [`fit_coxreg_multivar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md)
  (for multivariate models).

- ...:

  additional arguments for the lower level functions.

## Value

[`broom::tidy()`](https://broom.tidymodels.org/reference/reexports.html)
returns:

- For `summary.coxph` objects, a `data.frame` with columns: `Pr(>|z|)`,
  `exp(coef)`, `exp(-coef)`, `lower .95`, `upper .95`, `level`, and `n`.

- For `coxreg.univar` objects, a `data.frame` with columns: `effect`,
  `term`, `term_label`, `level`, `n`, `hr`, `lcl`, `ucl`, `pval`, and
  `ci`.

- For `coxreg.multivar` objects, a `data.frame` with columns: `term`,
  `pval`, `term_label`, `hr`, `lcl`, `ucl`, `level`, and `ci`.

## Functions

- `tidy(summary.coxph)`: Custom tidy method for
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
  summary results.

  Tidy the
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html)
  results into a `data.frame` to extract model results.

- `tidy(coxreg.univar)`: Custom tidy method for a univariate Cox
  regression.

  Tidy up the result of a Cox regression model fitted by
  [`fit_coxreg_univar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md).

- `tidy(coxreg.multivar)`: Custom tidy method for a multivariate Cox
  regression.

  Tidy up the result of a Cox regression model fitted by
  [`fit_coxreg_multivar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md).

## See also

[cox_regression](https://insightsengineering.github.io/tern/reference/cox_regression.md)

## Examples

``` r
library(survival)
library(broom)

set.seed(1, kind = "Mersenne-Twister")

dta_bladder <- with(
  data = bladder[bladder$enum < 5, ],
  data.frame(
    time = stop,
    status = event,
    armcd = as.factor(rx),
    covar1 = as.factor(enum),
    covar2 = factor(
      sample(as.factor(enum)),
      levels = 1:4, labels = c("F", "F", "M", "M")
    )
  )
)
labels <- c("armcd" = "ARM", "covar1" = "A Covariate Label", "covar2" = "Sex (F/M)")
formatters::var_labels(dta_bladder)[names(labels)] <- labels
dta_bladder$age <- sample(20:60, size = nrow(dta_bladder), replace = TRUE)

formula <- "survival::Surv(time, status) ~ armcd + covar1"
msum <- summary(coxph(stats::as.formula(formula), data = dta_bladder))
tidy(msum)
#>       Pr(>|z|) exp(coef) exp(-coef)  lower .95 upper .95   level   n
#> 1 1.287954e-02 0.6110123   1.636628 0.41442417 0.9008549  armcd2 340
#> 2 6.407916e-04 0.4460731   2.241785 0.28061816 0.7090818 covar12 340
#> 3 5.272933e-06 0.3075864   3.251119 0.18517346 0.5109230 covar13 340
#> 4 2.125359e-08 0.1808795   5.528541 0.09943722 0.3290258 covar14 340

## Cox regression: arm + 1 covariate.
mod1 <- fit_coxreg_univar(
  variables = list(
    time = "time", event = "status", arm = "armcd",
    covariates = "covar1"
  ),
  data = dta_bladder,
  control = control_coxreg(conf_level = 0.91)
)

## Cox regression: arm + 1 covariate + interaction, 2 candidate covariates.
mod2 <- fit_coxreg_univar(
  variables = list(
    time = "time", event = "status", arm = "armcd",
    covariates = c("covar1", "covar2")
  ),
  data = dta_bladder,
  control = control_coxreg(conf_level = 0.91, interaction = TRUE)
)

tidy(mod1)
#>            effect   term        term_label level   n        hr       lcl
#> ref    Treatment:  armcd  2 vs control (1)     2 340 0.6386426 0.4557586
#> covar1 Covariate: covar1 A Covariate Label     2 340  0.607037 0.4324571
#>              ucl       pval                   ci
#> ref    0.8949131 0.02423805 0.4557586, 0.8949131
#> covar1 0.8520935 0.01257339 0.4324571, 0.8520935
tidy(mod2)
#>                           effect   term        term_label level   n        hr
#> ref                   Treatment:  armcd  2 vs control (1)     2 340 0.6386426
#> covar1.1              Covariate: covar1 A Covariate Label       340          
#> covar1.armcd2/covar11 Covariate: covar1                 1     1     0.6284569
#> covar1.armcd2/covar12 Covariate: covar1                 2     2     0.5806499
#> covar1.armcd2/covar13 Covariate: covar1                 3     3     0.5486103
#> covar1.armcd2/covar14 Covariate: covar1                 4     4     0.6910725
#> covar2.1              Covariate: covar2         Sex (F/M)       340          
#> covar2.armcd2/covar2F Covariate: covar2                 F     F     0.6678243
#> covar2.armcd2/covar2M Covariate: covar2                 M     M     0.5954021
#>                             lcl       ucl       pval pval_inter
#> ref                   0.4557586 0.8949131 0.02423805           
#> covar1.1                     NA        NA             0.9883021
#> covar1.armcd2/covar11 0.3450471 1.1446499                      
#> covar1.armcd2/covar12 0.2684726 1.2558239                      
#> covar1.armcd2/covar13 0.2226814 1.3515868                      
#> covar1.armcd2/covar14 0.2308006 2.0692373                      
#> covar2.1                     NA        NA             0.7759013
#> covar2.armcd2/covar2F 0.3649842 1.2219413                      
#> covar2.armcd2/covar2M 0.3572772 0.9922368                      
#>                                         ci
#> ref                   0.4557586, 0.8949131
#> covar1.1                                  
#> covar1.armcd2/covar11 0.3450471, 1.1446499
#> covar1.armcd2/covar12 0.2684726, 1.2558239
#> covar1.armcd2/covar13 0.2226814, 1.3515868
#> covar1.armcd2/covar14 0.2308006, 2.0692373
#> covar2.1                                  
#> covar2.armcd2/covar2F 0.3649842, 1.2219413
#> covar2.armcd2/covar2M 0.3572772, 0.9922368

multivar_model <- fit_coxreg_multivar(
  variables = list(
    time = "time", event = "status", arm = "armcd",
    covariates = c("covar1", "covar2")
  ),
  data = dta_bladder
)
broom::tidy(multivar_model)
#>                       term         pval                        term_label
#> armcd.1              armcd                            ARM (reference = 1)
#> armcd.2                ARM   0.01232761                                 2
#> covar1.1            covar1 7.209956e-09 A Covariate Label (reference = 1)
#> covar1.2 A Covariate Label  0.001120332                                 2
#> covar1.3 A Covariate Label 6.293725e-06                                 3
#> covar1.4 A Covariate Label 3.013875e-08                                 4
#> covar2.1            covar2                      Sex (F/M) (reference = F)
#> covar2.2         Sex (F/M)    0.1910521                                 M
#>                 hr        lcl       ucl level                     ci
#> armcd.1                    NA        NA  <NA>                       
#> armcd.2  0.6062777 0.40970194 0.8971710     2   0.4097019, 0.8971710
#> covar1.1                   NA        NA  <NA>                       
#> covar1.2 0.4564763 0.28481052 0.7316115     2   0.2848105, 0.7316115
#> covar1.3 0.3069612 0.18386073 0.5124813     3   0.1838607, 0.5124813
#> covar1.4 0.1817017 0.09939435 0.3321668     4 0.09939435, 0.33216684
#> covar2.1                   NA        NA  <NA>                       
#> covar2.2  1.289373 0.88087820 1.8873019     M   0.8808782, 1.8873019
```
