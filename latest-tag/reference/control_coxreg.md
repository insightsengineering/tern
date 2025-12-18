# Control function for Cox regression

**\[stable\]**

Sets a list of parameters for Cox regression fit. Used internally.

## Usage

``` r
control_coxreg(
  pval_method = c("wald", "likelihood"),
  ties = c("exact", "efron", "breslow"),
  conf_level = 0.95,
  interaction = FALSE
)
```

## Arguments

- pval_method:

  (`string`)  
  the method used for estimation of p.values; `wald` (default) or
  `likelihood`.

- ties:

  (`string`)  
  among `exact` (equivalent to `DISCRETE` in SAS), `efron` and
  `breslow`, see
  [`survival::coxph()`](https://rdrr.io/pkg/survival/man/coxph.html).
  Note: there is no equivalent of SAS `EXACT` method in R.

- conf_level:

  (`proportion`)  
  confidence level of the interval.

- interaction:

  (`flag`)  
  if `TRUE`, the model includes the interaction between the studied
  treatment and candidate covariate. Note that for univariate models
  without treatment arm, and multivariate models, no interaction can be
  used so that this needs to be `FALSE`.

## Value

A `list` of items with names corresponding to the arguments.

## See also

[`fit_coxreg_univar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md)
and
[`fit_coxreg_multivar()`](https://insightsengineering.github.io/tern/reference/fit_coxreg.md).

## Examples

``` r
control_coxreg()
#> $pval_method
#> [1] "wald"
#> 
#> $ties
#> [1] "exact"
#> 
#> $conf_level
#> [1] 0.95
#> 
#> $interaction
#> [1] FALSE
#> 
```
